mod error;
mod execute;
mod explain;
mod process_response;
mod query;
mod query_usage;
mod steps;
mod to_opendd_ir;
mod types;

pub use error::RequestError;
pub use execute::{execute_mutation_plan, execute_query_plan, ExecuteQueryResult, RootFieldResult};
pub use explain::execute_explain;
pub use explain::types::{redact_ndc_explain, ExplainResponse};
pub use process_response::process_response;
pub use query::{
    execute_query, execute_query_internal, set_request_metadata_attributes, set_usage_attributes,
};
pub use steps::{build_ir, build_request_plan, generate_ir, normalize_request, parse_query};
pub use to_opendd_ir::to_opendd_ir;
pub use types::{GraphQLErrors, GraphQLResponse};

#[cfg(test)]
mod tests {
    use goldenfile::{differs::text_diff, Mint};
    use hasura_authn_core::{Identity, Role, Session, SessionVariableValue};
    use lang_graphql::http::Request;
    use lang_graphql::{parser::Parser, validation::normalize_request};
    use open_dds::session_variables::{SessionVariableName, SESSION_VARIABLE_ROLE};
    use serde_json as json;
    use std::{
        collections::HashMap,
        fs::{self, File},
        io::Write,
        path::PathBuf,
    };

    use crate::generate_ir;
    use crate::query_usage::analyze_query_usage;
    use graphql_schema::GDS;

    #[test]
    fn test_generate_ir() -> Result<(), Box<dyn std::error::Error>> {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut mint = Mint::new(&test_dir);

        let schema = fs::read_to_string(test_dir.join("schema.json"))?;

        let gds = GDS::new_with_default_flags(open_dds::Metadata::from_json_str(&schema)?)?;
        let schema = GDS::build_schema(&gds)?;

        for input_file in fs::read_dir(test_dir.join("generate_ir"))? {
            let path = input_file?.path();
            assert!(path.is_dir());

            let request_headers = reqwest::header::HeaderMap::new();
            let test_name = path
                .file_name()
                .ok_or_else(|| format!("{path:?} is not a normal file or directory"))?;

            let raw_request = fs::read_to_string(path.join("request.gql"))?;
            let expected_path = PathBuf::from("generate_ir")
                .join(test_name)
                .join("expected.json");

            let session_vars_path = path.join("session_variables.json");
            let session = resolve_session(session_vars_path);
            let query = Parser::new(&raw_request).parse_executable_document()?;

            let request = Request {
                operation_name: None,
                query,
                variables: HashMap::new(),
            };

            let normalized_request = normalize_request(
                &graphql_schema::GDSRoleNamespaceGetter {
                    scope: session.role.clone(),
                },
                &schema,
                &request,
            )?;

            let ir = generate_ir(&schema, &session, &request_headers, &normalized_request)?;
            let mut expected = mint.new_goldenfile_with_differ(
                expected_path,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2);
                    }
                }),
            )?;
            write!(expected, "{}", serde_json::to_string_pretty(&ir)?)?;
        }

        Ok(())
    }

    #[test]
    fn test_query_usage_analytics() -> Result<(), Box<dyn std::error::Error>> {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("query_usage_analytics");
        let mut mint = Mint::new(&test_dir);

        let schema = fs::read_to_string(test_dir.join("schema.json"))?;

        let gds = GDS::new_with_default_flags(open_dds::Metadata::from_json_str(&schema)?)?;
        let schema = GDS::build_schema(&gds)?;

        for test_dir in fs::read_dir(test_dir)? {
            let path = test_dir?.path();

            if !path.is_dir() {
                continue;
            }

            let test_name = path
                .file_name()
                .ok_or_else(|| format!("{path:?} is not a normal file or directory"))?;

            let raw_request = fs::read_to_string(path.join("request.gql"))?;
            let expected_path = PathBuf::from(test_name).join("expected.json");

            let session_vars_path = path.join("session_variables.json");
            let session = resolve_session(session_vars_path);
            let query = Parser::new(&raw_request).parse_executable_document()?;

            let request = Request {
                operation_name: None,
                query,
                variables: HashMap::new(),
            };

            let normalized_request = normalize_request(
                &graphql_schema::GDSRoleNamespaceGetter {
                    scope: session.role.clone(),
                },
                &schema,
                &request,
            )?;

            let query_usage = analyze_query_usage(&normalized_request);
            let mut expected = mint.new_goldenfile_with_differ(
                expected_path,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2);
                    }
                }),
            )?;
            write!(expected, "{}", serde_json::to_string_pretty(&query_usage)?)?;
        }
        Ok(())
    }

    // TODO: remove duplication between this function and 'add_session'
    fn resolve_session(session_vars_path: PathBuf) -> Session {
        let authorization = Identity::admin(Role::new("admin"));
        let session_variables: HashMap<SessionVariableName, String> = {
            if session_vars_path.exists() {
                json::from_str(fs::read_to_string(session_vars_path).unwrap().as_ref()).unwrap()
            } else {
                HashMap::new()
            }
        };

        let role = session_variables
            .get(&SESSION_VARIABLE_ROLE)
            .map(|v| Role::new(v));
        let session_variables = session_variables
            .into_iter()
            .map(|(k, v)| (k, SessionVariableValue::Unparsed(v)))
            .collect();
        authorization
            .get_role_authorization(role.as_ref())
            .unwrap()
            .build_session(session_variables)
    }
}
