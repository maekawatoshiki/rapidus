#!/usr/bin/env python3
"""Run a practical subset of test262 files through rapidus.

This is intentionally small and repository-local. It is not a replacement for
the official test262 harness; it gives rapidus contributors a repeatable
baseline and a way to sort failures by broad cause.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
from typing import Any


ROOT = Path(__file__).resolve().parents[1]
TEST262 = ROOT / "test262"
HARNESS = TEST262 / "harness"
DEFAULT_PATHS = [
    TEST262 / "test" / "language",
    TEST262 / "test" / "built-ins",
    TEST262 / "test" / "annexB",
]


FRONTMATTER_RE = re.compile(r"/\*---(?P<body>.*?)---\*/", re.S)
ERROR_MARKERS = ("Syntax error", "Runtime error", "Uncaught Exception", "panicked at")


def parse_list(value: str) -> list[str]:
    value = value.strip()
    if not (value.startswith("[") and value.endswith("]")):
        return []
    inner = value[1:-1].strip()
    if not inner:
        return []
    return [part.strip().strip("\"'") for part in inner.split(",") if part.strip()]


def parse_block_list(lines: list[str], index: int, value: str) -> tuple[list[str], int]:
    parsed = parse_list(value)
    if parsed or value.strip() == "[]":
        return parsed, index

    items: list[str] = []
    i = index + 1
    bracketed = value.strip() == "["
    while i < len(lines):
        stripped = lines[i].strip()
        if not stripped:
            i += 1
            continue
        if bracketed and stripped == "]":
            return items, i
        if not lines[i].startswith(" ") and ":" in stripped:
            return items, i - 1
        if stripped.startswith("- "):
            items.append(stripped[2:].strip().strip("\"'"))
        elif bracketed:
            item = stripped.rstrip(",").strip("\"'")
            if item:
                items.append(item)
        i += 1
    return items, i - 1


def read_metadata(path: Path) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8", errors="replace")
    match = FRONTMATTER_RE.search(text)
    meta: dict[str, Any] = {
        "includes": [],
        "flags": [],
        "features": [],
        "negative": None,
        "raw": False,
    }
    if not match:
        return meta

    lines = match.group("body").splitlines()
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if not line or line.startswith("#") or ":" not in line:
            i += 1
            continue
        key, value = line.split(":", 1)
        key = key.strip()
        value = value.strip()
        if key in {"includes", "flags", "features"}:
            meta[key], i = parse_block_list(lines, i, value)
        elif key == "negative":
            negative: dict[str, str] = {}
            i += 1
            while i < len(lines):
                sub = lines[i]
                if sub.strip() and not sub.startswith(" ") and ":" in sub:
                    i -= 1
                    break
                if ":" in sub:
                    nkey, nvalue = sub.split(":", 1)
                    negative[nkey.strip()] = nvalue.strip()
                i += 1
            meta["negative"] = negative
        i += 1

    meta["raw"] = "raw" in meta["flags"]
    return meta


def harness_source(includes: list[str], raw: bool) -> str:
    if raw:
        return ""
    parts = []
    for name in ["sta.js", "assert.js", *includes]:
        path = HARNESS / name
        try:
            parts.append(path.read_text(encoding="utf-8"))
        except FileNotFoundError:
            parts.append(f"throw new Test262Error('missing harness include: {name}');")
    return "\n".join(parts) + "\n"


def error_kind(output: str, returncode: int = 0) -> str | None:
    if "Syntax error" in output:
        return "parse"
    if "Runtime error" in output or "Uncaught Exception" in output or "panicked at" in output:
        return "runtime"
    if returncode != 0:
        return "runtime"
    return None


def expected_negative_kind(meta: dict[str, Any]) -> str | None:
    negative = meta.get("negative")
    if not negative:
        return None
    phase = negative.get("phase", "")
    if phase == "runtime":
        return "runtime"
    return "parse"


def iter_tests(paths: list[Path]) -> list[Path]:
    def is_test(path: Path) -> bool:
        return path.suffix == ".js" and "FIXTURE" not in path.name

    tests: list[Path] = []
    for path in paths:
        if not path.is_absolute():
            path = ROOT / path
        if path.is_file() and is_test(path):
            tests.append(path)
        elif path.is_dir():
            tests.extend(child for child in sorted(path.rglob("*.js")) if is_test(child))
    return tests


def build_source(path: Path, meta: dict[str, Any]) -> str:
    body = path.read_text(encoding="utf-8", errors="replace")
    prefix = ""
    if "onlyStrict" in meta["flags"]:
        prefix += '"use strict";\n'
    prefix += harness_source(meta["includes"], meta["raw"])
    suffix = ""
    if not meta["raw"] and "async" in meta["flags"]:
        prefix += (
            "var $DONE_CALLED = false;\n"
            "function $DONE(error) {\n"
            "  if (error) { throw error; }\n"
            "  $DONE_CALLED = true;\n"
            "}\n"
        )
        suffix = (
            "\n__drain_promise_jobs();\n"
            "if (!$DONE_CALLED) { throw new Test262Error('$DONE was not called'); }\n"
        )
    return prefix + body + suffix


def run_one(
    path: Path,
    rapidus: Path,
    timeout: float,
    workdir: Path,
    skip_features: set[str],
    skip_flags: set[str],
) -> dict[str, Any]:
    meta = read_metadata(path)
    rel = path.relative_to(ROOT).as_posix()
    skipped_features = sorted(set(meta["features"]) & skip_features)
    skipped_flags = sorted(set(meta["flags"]) & skip_flags)
    if skipped_features or skipped_flags:
        return {
            "path": rel,
            "pass": False,
            "skip": True,
            "reason": "skipped",
            "skip_features": skipped_features,
            "skip_flags": skipped_flags,
            "flags": meta["flags"],
            "features": meta["features"],
            "includes": meta["includes"],
            "negative": meta["negative"],
            "output": "",
        }

    source = build_source(path, meta)
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", suffix=".js", dir=workdir, delete=False
    ) as temp:
        temp.write(source)
        temp_path = Path(temp.name)
    try:
        command = [str(rapidus)]
        if "module" in meta["flags"]:
            command.append("--module")
        command.append(str(temp_path))
        proc = subprocess.run(
            command,
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            timeout=timeout,
        )
        output = proc.stdout
        actual = error_kind(output, proc.returncode)
        expected = expected_negative_kind(meta)
        if expected is None:
            passed = actual is None
            reason = "ok" if passed else f"unexpected-{actual or 'unknown-error'}"
        else:
            passed = actual == expected
            reason = "ok" if passed else f"expected-{expected}-got-{actual or 'none'}"
        return {
            "path": rel,
            "pass": passed,
            "skip": False,
            "reason": reason,
            "flags": meta["flags"],
            "features": meta["features"],
            "includes": meta["includes"],
            "negative": meta["negative"],
            "output": output[-1000:],
        }
    except subprocess.TimeoutExpired:
        return {
            "path": rel,
            "pass": False,
            "skip": False,
            "reason": "timeout",
            "flags": meta["flags"],
            "features": meta["features"],
            "includes": meta["includes"],
            "negative": meta["negative"],
            "output": "",
        }
    finally:
        try:
            temp_path.unlink()
        except FileNotFoundError:
            pass


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", nargs="*", type=Path, default=DEFAULT_PATHS)
    parser.add_argument("--rapidus", type=Path, default=ROOT / "target/debug/rapidus-repl")
    parser.add_argument("--build", action="store_true")
    parser.add_argument("--limit", type=int)
    parser.add_argument("--offset", type=int, default=0)
    parser.add_argument("--jobs", type=int, default=max(1, min(os.cpu_count() or 1, 8)))
    parser.add_argument("--timeout", type=float, default=2.0)
    parser.add_argument("--json", type=Path)
    parser.add_argument("--failures", type=int, default=20)
    parser.add_argument("--skip-feature", action="append", default=[])
    parser.add_argument("--skip-flag", action="append", default=[])
    args = parser.parse_args()

    if args.build:
        subprocess.run(["cargo", "build", "-p", "rapidus-repl"], cwd=ROOT, check=True)

    tests = iter_tests(args.paths)
    if args.offset < 0:
        parser.error("--offset must be non-negative")
    if args.offset:
        tests = tests[args.offset :]
    if args.limit is not None:
        tests = tests[: args.limit]

    workdir = Path(tempfile.mkdtemp(prefix="rapidus-test262-"))
    results: list[dict[str, Any]] = []
    skip_features = set(args.skip_feature)
    skip_flags = set(args.skip_flag)
    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
            futures = [
                executor.submit(
                    run_one,
                    path,
                    args.rapidus,
                    args.timeout,
                    workdir,
                    skip_features,
                    skip_flags,
                )
                for path in tests
            ]
            for future in concurrent.futures.as_completed(futures):
                results.append(future.result())
    finally:
        for child in workdir.glob("*"):
            child.unlink(missing_ok=True)
        workdir.rmdir()

    results.sort(key=lambda item: item["path"])
    skipped = sum(1 for item in results if item.get("skip"))
    run_results = [item for item in results if not item.get("skip")]
    passed = sum(1 for item in run_results if item["pass"])
    total = len(results)
    run_total = len(run_results)
    rate = (passed / run_total * 100.0) if run_total else 0.0
    by_reason: dict[str, int] = {}
    for item in run_results:
        if item["pass"]:
            continue
        by_reason[item["reason"]] = by_reason.get(item["reason"], 0) + 1

    print(
        f"total={total} run={run_total} skipped={skipped} "
        f"passed={passed} failed={run_total - passed} pass_rate={rate:.2f}%"
    )
    for reason, count in sorted(by_reason.items(), key=lambda pair: pair[1], reverse=True):
        print(f"  {reason}: {count}")
    if args.failures:
        print("sample failures:")
        shown = 0
        for item in run_results:
            if item["pass"]:
                continue
            print(f"  {item['reason']}: {item['path']}")
            shown += 1
            if shown >= args.failures:
                break

    if args.json:
        args.json.write_text(json.dumps(results, indent=2, ensure_ascii=False), encoding="utf-8")
    return 0 if run_total and passed == run_total else 1


if __name__ == "__main__":
    raise SystemExit(main())
