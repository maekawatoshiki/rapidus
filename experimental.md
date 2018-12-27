## 主な変更点

### 1.Value構造体

- 従来の `Value`, `ValueBase` 構造体をそれぞれ `Property`, `Value` 構造体へ変更した。
writable, configurable などの属性は Property のメンバとなる。
- Valueは`to_property(&self)`で`Property`に変換できる。
- 識別子とProperyのタプルとして`NamePropPair`, プロパティのmapとして`PropMap`を定義した。

```Rust
pub type NamePropPair = (String, Property);
pub type PropMap = GcType<FxHashMap<String, Property>>;
```

- `Value::propmap_from_npp(&Vec<NamePropPair>)`で`PropMap`、`Value::object_from_npp(&Vec<NamePropPair)`で`Value::Object`、`Value::array_from_elems(Vec<Value>)`で`Value::Array`を生成でき,ハッシュマップやGCの実装を隠蔽できる。
- make_npp!マクロとmake_object!マクロにより`NamePropPair`と`Value::Object`の生成が行える。
- ただ、マクロを多用するとエラー表示が分かりづらくなる（マクロ定義の箇所におけるエラーとなるっぽい）ので、あまり大々的には使用しないほうがいいかも。

```Rust
pub fn propmap_from_npp(npp: &Vec<NamePropPair>) -> PropMap {
    let mut map = FxHashMap::default();
    for p in npp {
        map.insert(p.0.clone(), p.1.clone());
    }
    gc::new(map)
}
pub fn object_from_npp(npp: &Vec<NamePropPair>) -> Value {
    let map = Value::propmap_from_npp(&npp);
    Value::object(map)
}

pub fn array_from_elems(elms: Vec<Value>) -> Value {
    let ary = ArrayValue::new(elms);
    Value::Array(gc::new(ary))
}

let obj: Value = make_object!(
    message:    Value::string(message),
    name:       Value::string("Error".to_string()),
    __proto__:  make_object!()
);

let map: PropMap = Value::propmap_from_npp(&make_npp!(
    push:   Value::default_builtin_function(prototype_push),
    pop:    Value::default_builtin_function(prototype_pop),
    map:    Value::default_builtin_function(prototype_map)
));
```

### 2.vm.state.history

- `vm.state.history` は従来タプル `(sp, pc, FuncId)` を `vm.do_run()`を呼ぶ側で退避、`do_run()`の実行ループから呼ばれるバイトコード実行関数内で復帰を行なっていたのを、`do_run()`の内部で退避・復帰を完結するようにロジックを変更した。
また、`FuncId` をタプルから削除。

### 3.builtinモジュールAPI

- builtinオブジェクトはvm起動時にモジュール内の`init() -> Value`を呼ぶことで初期化するようにした。モジュールは`init()`内でプロトタイプ・オブジェクト生成等の初期化処理を行い、コンストラクタ関数を返却し、vmがグルーバルオブジェクト（現状ではグローバル`CallObject`）に登録する。
- `init()`に関して、JIT用のコードを持つモジュールに関しては`TracingJit`を渡す必要があったり、複数のグローバルオブジェクトを返すモジュール（`Error`など）もあるので、今の所一貫したAPIはない。

### 4.JIT ON/OFF

- パフォーマンス測定用に`__enableJit(bool)`でJITをON/OFFできるようにした。
（`examples/fibo.js`参照）

### 5.trace オプション

- コマンドラインから `--trace` オプションをつけて実行するとバイトコード実行・スタックの状態をトレースするようにした（REPL でも使用可）。

- 左から順に `trystate_stack` のスタックトップの `TryState` および `TryReturn`, `state.scope` の要素数、`state.stack` の要素数、`stack` スタックトップの `Value`, プログラムカウンタ、VM インストラクションを表示する。
`stack` の要素数が 0 の時は EMPTY と表示する。

- また、PUSH_INT や JMP、CALL など引数を持つ一部のインストラクションについては引数も表示するようにした。

- また、関数終了（`do_run()`脱出）のタイミングで stack スタックの内容を出力する。
（"stack trace"）

```
$ cargo run -- --trace
    Finished dev [unoptimized + debuginfo] target(s) in 0.23s
     Running `target/debug/rapidus --trace`
> try { throw 123 } catch(e) { return 1 } finally { throw 45 }
 tryst    tryret   scope stack   top            PC   INST
 None              1     0     EMPTY           0000 CreateContext
 None              1     0     EMPTY           0001 EnterTry
 Try      None     1     0     EMPTY           000a PushInt8 123
 Try      None     1     1     123             000c Throw
 Catch    None     1     1     123             0012 Catch
 Catch    None     1     1     123             0013 PushScope
 Catch    None     2     1     123             0014 DeclVar
 Catch    None     2     1     123             0019 SetValue
 Catch    None     2     0     EMPTY           001e PushInt8 1
 Catch    None     2     1     1               0020 PopScope
 Catch    None     1     1     1               0021 ReturnTry
 Finally  1        1     0     EMPTY           0027 Finally
 Finally  1        1     0     EMPTY           0028 PushInt8 45
 Finally  1        1     1     45              002a Throw
stack trace:[undefined]
runtime error: Uncaught Exception
45
```

```
$ cargo run -- --trace
    Finished dev [unoptimized + debuginfo] target(s) in 0.96s
     Running `target/debug/rapidus --trace`
> 1;2;3;4;5
 tryst    tryret   scope stack   top            PC   INST
 None              1     0     EMPTY           0000 CreateContext
 None              1     0     EMPTY           0001 PushInt8 1
 None              1     1     1               0003 PushInt8 2
 None              1     2     2               0005 PushInt8 3
 None              1     3     3               0007 PushInt8 4
 None              1     4     4               0009 PushInt8 5
 None              1     5     5               000b End
stack trace:[1][2][3][4][5]
5
```
```
$ cargo run -- --trace
    Finished dev [unoptimized + debuginfo] target(s) in 1.17s
     Running `target/debug/rapidus --trace`
> if (true) { var a = [1,2,3] }
 tryst    tryret   scope stack   top            PC   INST
 None              1     0     EMPTY           0000 CreateContext
 None              1     0     EMPTY           0001 DeclVar
 None              1     0     EMPTY           0006 PushTrue
 None              1     1     true            0007 JmpIfFalse 001c
 None              1     0     EMPTY           000c PushInt8 3
 None              1     1     3               000e PushInt8 2
 None              1     2     2               0010 PushInt8 1
 None              1     3     1               0012 CreateArray 3 params
 None              1     1     1,2,3           0017 SetValue
 None              1     0     EMPTY           001c End
stack trace:
undefined
```