use rapidus_eval::{bytecode::compiler::ModuleCompiler, value::JsValue};
use rapidus_parser::{
    lexer::{Input, Lexer},
    parser::Parser,
    source::{Source, SourceName},
};

#[test]
fn bytecode_num() {
    let src = Source::new(SourceName::Custom("test".into()), "123");
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let mut ctx = ModuleCompiler::new().compile(&module).unwrap();
    let val = ctx.run().unwrap();
    assert_eq!(val, JsValue::f64(123.0));
}

#[test]
fn bytecode_arith() {
    let src = Source::new(SourceName::Custom("test".into()), r#"12345+19382-113*91/2"#);
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let mut ctx = ModuleCompiler::new().compile(&module).unwrap();
    let val = ctx.run().unwrap();
    assert_eq!(val, JsValue::f64(26585.5));
}

#[test]
fn bytecode_str() {
    let src = Source::new(SourceName::Custom("test".into()), r#""hello""#);
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let mut ctx = ModuleCompiler::new().compile(&module).unwrap();
    let val = ctx.run().unwrap();
    assert_eq!(format!("{val:?}"), r#"str("hello")"#);
}

#[test]
fn bytecode_let_binding() {
    let src = Source::new(
        SourceName::Custom("test".into()),
        r#"let foo = 24; let bar = 18; foo + bar"#,
    );
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let mut ctx = ModuleCompiler::new().compile(&module).unwrap();
    let val = ctx.run().unwrap();
    assert_eq!(val, JsValue::f64(42.0));
}

#[test]
fn bytecode_null() {
    let src = Source::new(SourceName::Custom("test".into()), r#"null"#);
    let module = Parser::new(Lexer::new(Input::from(&src)))
        .parse_module()
        .unwrap();
    let mut ctx = ModuleCompiler::new().compile(&module).unwrap();
    let val = ctx.run().unwrap();
    assert_eq!(val, JsValue::null());
}
