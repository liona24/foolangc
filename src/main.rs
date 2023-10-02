use std::io::Read;

mod codegen;
mod error;
mod parse_types;
mod parsing;
mod string_pool;
mod tree_simplification;

mod foolang;

use string_pool::StringPool;

fn main() {
    let mut strings = StringPool::new();
    let grammar = foolang::create_grammar(strings);
    let builder = foolang::FooLangLexerBuilder::new();

    let mut input = Vec::new();
    std::io::stdin()
        .read_to_end(&mut input)
        .expect("error while reading input");
    let input = String::from_utf8(input).expect("invalid input!");

    let mut lexer = builder.lex(&input);
    let forest = crate::parsing::parse(&mut lexer, &grammar, &mut strings)
        .map_err(|err| err.raise(&lexer))
        .unwrap();

    /*
    use std::io::Write;
    let mut f = std::fs::File::create("dump.dot").unwrap();
    f.write_all(forest.dump(strings).as_bytes()).unwrap();
    */

    let tree = crate::tree_simplification::ParseTree::new(&forest, strings)
        .map_err(|err| err.raise(&lexer))
        .unwrap();

    /*
    let mut f = std::fs::File::create("dump_simple.dot").unwrap();
    f.write_all(tree.dump(strings).as_bytes()).unwrap();
    */

    let ir = crate::codegen::ir_translation::translate(&tree, strings);

    {
        let f = std::fs::File::create("code.s").unwrap();
        crate::codegen::generate(f, &ir, strings).expect("could not write output!");
    }

    std::process::Command::new("/bin/sh")
        .arg("-c")
        .arg("as -o code.o code.s && ld -o code code.o")
        .spawn()
        .expect("compiler error!");
}
