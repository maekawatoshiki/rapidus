use rapidus_eval::{eval::EvalCtx, value::JsValue};
use rapidus_parser::{
    lexer::{Input, Lexer},
    parser::Parser,
    source::{Source, SourceName},
};

#[test]
fn num() {
    let src = Source::new(SourceName::Custom("test".into()), "123");
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let val = EvalCtx::new().eval_module(&module).unwrap();
    assert_eq!(val, JsValue::f64(123.0));
}

#[test]
fn str() {
    let src = Source::new(SourceName::Custom("test".into()), r#""hello""#);
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let val = EvalCtx::new().eval_module(&module).unwrap();
    assert_eq!(format!("{val:?}"), r#"str("hello")"#);
}

#[test]
fn null() {
    let src = Source::new(SourceName::Custom("test".into()), r#"null"#);
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let val = EvalCtx::new().eval_module(&module).unwrap();
    assert_eq!(val, JsValue::null());
}

#[test]
fn arith() {
    let src = Source::new(SourceName::Custom("test".into()), r#"12345+19382-113*91/2"#);
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let val = EvalCtx::new().eval_module(&module).unwrap();
    assert_eq!(val, JsValue::f64(26585.5));
}

#[test]
fn let_binding() {
    let src = Source::new(
        SourceName::Custom("test".into()),
        r#"let foo = 24; let bar = 18; foo + bar"#,
    );
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let val = EvalCtx::new().eval_module(&module).unwrap();
    assert_eq!(val, JsValue::f64(42.0));
}
