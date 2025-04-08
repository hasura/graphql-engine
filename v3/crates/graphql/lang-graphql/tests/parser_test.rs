use indexmap::IndexMap;
/// Test both schema and query parsing
use lang_graphql::parser;
use std::io;
use std::{
    env, fs,
    path::{Path, PathBuf},
};

use expect_test::expect_file;

#[cfg(test)]
#[allow(clippy::print_stdout)]
fn test_parser_for_schema(schema_path: &Path) -> Result<(), io::Error> {
    let schema = fs::read_to_string(schema_path)?;
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
                    panic!("Parsing error:\n{err:#?}");
                }
                Ok(document) => {
                    let actual_ast = format!("{document:#?}");
                    let patch = diffy::create_patch(expected_ast.as_str(), actual_ast.as_str());
                    // No diff
                    if patch.hunks().is_empty() {
                        return Ok(());
                    }
                    println!("AST diff:\n{patch}");

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

/// Test SDL parsing. For each .graphql file with an accompanying .ast.txt file, make sure
/// parse_schema_document() of the former results in the latter.
#[test]
fn test_schema_parser() -> Result<(), io::Error> {
    let mut testdata = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    testdata.push("tests");
    testdata.push("testdata");
    for dir_entry in fs::read_dir(testdata)? {
        let dir_entry = dir_entry?;
        if matches!(
            dir_entry.path().extension().map(|e| e.to_str()),
            Some(Some("graphql"))
        ) {
            test_parser_for_schema(&dir_entry.path())?;
        }
    }
    Ok(())
}

#[test]
fn query_parser_wellformed_input_tests() {
    dir_tests(&test_data_dir(), &["ok"], "txt", |text, path| {
        let cst = parser::Parser::new(text).parse_executable_document();
        assert_is_ok(&cst, path);
        format!("{cst:#?}")
    });
}

#[test]
fn query_parser_malformed_input_tests() {
    dir_tests(&test_data_dir(), &["err"], "txt", |text, path| {
        let cst = parser::Parser::new(text).parse_executable_document();
        assert_is_err(&cst, path);
        format!("{cst:#?}")
    });
}

// -----------------------------------------------------------------------------------------------
// Code below was copied or adapted from the apollo-rs project at 721e0753 and the license at
// `lang-graphql/tests/LICENSE-MIT` applies.

/// Compares input code taken from a `.graphql` file in test_fixtures and its
/// expected output in the corresponding `.txt` file.
///
/// The test fails if the ouptut differs.
///
/// If a matching file does not exist, it will be created, filled with output,
/// but fail the test.
fn dir_tests<F>(test_data_dir: &Path, paths: &[&str], outfile_extension: &str, f: F)
where
    F: Fn(&str, &Path) -> String,
{
    for (path, input_code) in collect_graphql_files(test_data_dir, paths) {
        let actual = f(&input_code, &path);
        let path = path.with_extension(outfile_extension);
        // TODO: we really want formatted output here, but:
        // https://github.com/rust-analyzer/expect-test/issues/45
        expect_file![path].assert_eq(&actual);
    }
}

/// Collects all `.graphql` files from `dir` subdirectories defined by `paths`.
fn collect_graphql_files(root_dir: &Path, paths: &[&str]) -> Vec<(PathBuf, String)> {
    paths
        .iter()
        .flat_map(|path| {
            let path = root_dir.to_owned().join(path);
            graphql_files_in_dir(&path).into_iter()
        })
        .map(|path| {
            let text = fs::read_to_string(&path)
                .unwrap_or_else(|_| panic!("File at {path:?} should be valid"));
            (path, text)
        })
        .collect()
}

/// Collects paths to all `.graphql` files from `dir` in a sorted `Vec<PathBuf>`.
fn graphql_files_in_dir(dir: &Path) -> Vec<PathBuf> {
    let mut paths = fs::read_dir(dir)
        .unwrap()
        .map(|file| {
            let file = file?;
            let path = file.path();
            if path.extension().unwrap_or_default() == "graphql" {
                Ok(Some(path))
            } else {
                Ok(None)
            }
        })
        // Get rid of the `None`s
        .filter_map(|result: std::io::Result<_>| result.transpose())
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    paths.sort();

    // Check for duplicate numbers.
    let mut seen = IndexMap::new();
    let next_number = paths.len() + 1;
    for path in &paths {
        let file_name = path.file_name().unwrap().to_string_lossy();
        let (number, name): (usize, _) = match file_name.split_once('_') {
            Some((number, name)) => match number.parse() {
                Ok(number) => (number, name),
                Err(err) => {
                    panic!("Invalid test file name: {path:?} does not start with a number ({err})")
                }
            },
            None => panic!("Invalid test file name: {path:?} does not start with a number"),
        };

        if let Some(existing) = seen.get(&number) {
            let suggest = dir.join(format!("{next_number:03}_{name}"));
            panic!(
                "Conflicting test file: {path:?} has the same number as {existing:?}. Suggested name: {suggest:?}"
            );
        }

        seen.insert(number, path);
    }

    paths
}

/// PathBuf of test cases directory.
fn test_data_dir() -> PathBuf {
    project_root().join("lang-graphql/tests/query_testdata")
}

/// project root.
fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

// Additional sanity checks to distinguish our error and success expectations:
#[allow(clippy::print_stdout)]
fn assert_is_err<T: std::fmt::Debug>(actual: &parser::Result<T>, path: &Path) {
    if actual.is_ok() {
        println!("erroneously successful parse: {actual:?}");
        panic!(
            "There should be errors in the file since this is an error case, but saw none in {:?}",
            path.display()
        );
    }
}

#[allow(clippy::print_stdout)]
fn assert_is_ok<T: std::fmt::Debug>(actual: &parser::Result<T>, path: &Path) {
    if actual.is_err() {
        println!("error: {actual:?}");
        panic!("There should be no errors in the file {:?}", path.display(),);
    }
}
