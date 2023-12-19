use lang_graphql::parser;
use std::env;
use std::fs;
use std::io;
use std::path::PathBuf;

#[cfg(test)]
fn test_parser_for_schema(schema_path: PathBuf) -> Result<(), io::Error> {
    let schema = fs::read_to_string(schema_path.as_path())?;
    let expected_ast_path = schema_path.with_extension("ast.txt");
    match fs::read_to_string(expected_ast_path.as_path()) {
        Err(io_error) => {
            // If the expected AST file doesn't exist, then just skip this schema
            if !matches!(io_error.kind(), io::ErrorKind::NotFound) {
                return Err(io_error);
            }
        }
        Ok(expected_ast) => {
            println!("Testing parser for schema: {}", schema_path.display());
            let mut parser = parser::Parser::new(&schema);
            match parser.parse_schema_document() {
                Err(err) => {
                    panic!("Parsing error:\n{:#?}", err);
                }
                Ok(document) => {
                    let actual_ast = format!("{:#?}", document);
                    let patch = diffy::create_patch(expected_ast.as_str(), actual_ast.as_str());
                    // No diff
                    if patch.hunks().is_empty() {
                        return Ok(());
                    }
                    println!("AST diff:\n{}", patch);

                    // Also write the actual AST to a temp file, to make it easy to update the golden file if the new output is expected
                    let mut tmp_output = env::temp_dir();
                    tmp_output.push(expected_ast_path.file_name().unwrap());
                    println!("Writing full AST to {}", tmp_output.display());
                    fs::write(tmp_output, actual_ast)?;
                    panic!();
                }
            }
        }
    }
    Ok(())
}

#[test]
fn test_parser() -> Result<(), io::Error> {
    let mut testdata = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    testdata.push("tests");
    testdata.push("testdata");
    for dir_entry in fs::read_dir(testdata)? {
        let dir_entry = dir_entry?;
        if matches!(
            dir_entry.path().extension().map(|e| e.to_str()),
            Some(Some("graphql"))
        ) {
            test_parser_for_schema(dir_entry.path())?;
        }
    }
    Ok(())
}
