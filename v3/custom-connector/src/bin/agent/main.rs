use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashSet},
    error::Error,
    fs::File,
    io::{self, BufRead},
    sync::Arc,
};

use axum::{
    extract::State,
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};

use indexmap::IndexMap;
use ndc_client::models::{self, Query};
use prometheus::{Encoder, IntCounter, IntGauge, Opts, Registry, TextEncoder};
use regex::Regex;
use serde_json::Value;
use tokio::sync::Mutex;

// ANCHOR: row-type
type Row = BTreeMap<String, serde_json::Value>;
// ANCHOR_END: row-type
// ANCHOR: app-state
#[derive(Debug, Clone)]
pub struct AppState {
    pub actors: BTreeMap<i64, Row>,
    pub movies: BTreeMap<i64, Row>,
    pub institutions: BTreeMap<i64, Row>,
    pub metrics: Metrics,
}
// ANCHOR_END: app-state

// ANCHOR: read_json_lines
fn read_json_lines(path: &str) -> core::result::Result<BTreeMap<i64, Row>, Box<dyn Error>> {
    let file = File::open(path)?;
    let lines = io::BufReader::new(file).lines();
    let mut records: BTreeMap<i64, Row> = BTreeMap::new();
    for line in lines {
        let row: BTreeMap<String, serde_json::Value> = serde_json::from_str(&line?)?;
        let id = row
            .get("id")
            .ok_or("'id' field not found in json file")?
            .as_i64()
            .ok_or("'id' field was not an integer in json file")?;
        records.insert(id, row);
    }
    Ok(records)
}
// ANCHOR_END: read_json_lines

#[derive(Debug, Clone)]
pub struct Metrics {
    pub registry: Registry,
    pub total_requests: IntCounter,
    pub active_requests: IntGauge,
}

impl Metrics {
    fn new() -> prometheus::Result<Metrics> {
        let total_requests =
            IntCounter::with_opts(Opts::new("total_requests", "number of total requests"))?;
        let active_requests =
            IntGauge::with_opts(Opts::new("active_requests", "number of active requests"))?;
        let registry = Registry::new();
        registry.register(Box::new(total_requests.clone()))?;
        registry.register(Box::new(active_requests.clone()))?;
        Ok(Metrics {
            registry,
            total_requests,
            active_requests,
        })
    }

    fn as_text(&self) -> Option<String> {
        let mut buffer = vec![];
        let encoder = TextEncoder::new();
        let metric_families = self.registry.gather();
        encoder.encode(&metric_families, &mut buffer).ok()?;
        String::from_utf8(buffer).ok()
    }
}

// ANCHOR: metrics_middleware
async fn metrics_middleware<T>(
    state: State<Arc<Mutex<AppState>>>,
    request: axum::http::Request<T>,
    next: axum::middleware::Next<T>,
) -> axum::response::Response {
    // Don't hold the lock to update metrics, since the
    // lock doesn't protect the metrics anyway.
    let metrics = {
        let state = state.lock().await;
        state.metrics.clone()
    };

    metrics.total_requests.inc();
    metrics.active_requests.inc();
    let response = next.run(request).await;
    metrics.active_requests.dec();
    response
}
// ANCHOR_END: metrics_middleware
// ANCHOR: init_app_state
fn init_app_state() -> AppState {
    // Read the CSV data files
    let actors = read_json_lines("./custom-connector/src/actors.json").unwrap();
    let movies = read_json_lines("./custom-connector/src/movies.json").unwrap();
    let institutions = read_json_lines("./custom-connector/src/institutions.json").unwrap();

    let metrics = Metrics::new().unwrap();

    AppState {
        actors,
        movies,
        institutions,
        metrics,
    }
}
// ANCHOR_END: init_app_state

type Result<A> = core::result::Result<A, (StatusCode, Json<models::ErrorResponse>)>;

// ANCHOR: main
#[tokio::main]
async fn main() {
    let app_state = Arc::new(Mutex::new(init_app_state()));

    let app = Router::new()
        .route("/healthz", get(get_healthz))
        .route("/metrics", get(get_metrics))
        .route("/capabilities", get(get_capabilities))
        .route("/schema", get(get_schema))
        .route("/query", post(post_query))
        .route("/mutation", post(post_mutation))
        .route("/explain", post(post_explain))
        .layer(axum::middleware::from_fn_with_state(
            app_state.clone(),
            metrics_middleware,
        ))
        .with_state(app_state);

    // run it with hyper on localhost:8101
    axum::Server::bind(&"0.0.0.0:8101".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
// ANCHOR_END: main
// ANCHOR: health
async fn get_healthz() -> StatusCode {
    StatusCode::NO_CONTENT
}
// ANCHOR_END: health
// ANCHOR: metrics
async fn get_metrics(State(state): State<Arc<Mutex<AppState>>>) -> Result<String> {
    let state = state.lock().await;
    state.metrics.as_text().ok_or((
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(models::ErrorResponse {
            message: "cannot encode metrics".into(),
            details: serde_json::Value::Null,
        }),
    ))
}
// ANCHOR_END: metrics
// ANCHOR: capabilities
async fn get_capabilities() -> Json<models::CapabilitiesResponse> {
    Json(models::CapabilitiesResponse {
        version: "0.1.0".into(),
        capabilities: models::Capabilities {
            mutation: models::MutationCapabilities {
                transactional: None,
                explain: None,
            },
            query: models::QueryCapabilities {
                explain: None,
                aggregates: Some(models::LeafCapability {}),
                variables: Some(models::LeafCapability {}),
            },
            relationships: Some(models::RelationshipCapabilities {
                relation_comparisons: Some(models::LeafCapability {}),
                order_by_aggregate: Some(models::LeafCapability {}),
            }),
        },
    })
}
// ANCHOR_END: capabilities
// ANCHOR: schema1
async fn get_schema() -> Json<models::SchemaResponse> {
    // ANCHOR_END: schema1
    // ANCHOR: schema_scalar_types
    let scalar_types = BTreeMap::from_iter([
        (
            "String".into(),
            models::ScalarType {
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::from_iter([(
                    "like".into(),
                    models::ComparisonOperatorDefinition::Custom {
                        argument_type: models::Type::Named {
                            name: "String".into(),
                        },
                    },
                )]),
            },
        ),
        (
            "Int".into(),
            models::ScalarType {
                aggregate_functions: BTreeMap::from_iter([
                    (
                        "max".into(),
                        models::AggregateFunctionDefinition {
                            result_type: models::Type::Nullable {
                                underlying_type: Box::new(models::Type::Named {
                                    name: "Int".into(),
                                }),
                            },
                        },
                    ),
                    (
                        "min".into(),
                        models::AggregateFunctionDefinition {
                            result_type: models::Type::Nullable {
                                underlying_type: Box::new(models::Type::Named {
                                    name: "Int".into(),
                                }),
                            },
                        },
                    ),
                ]),
                comparison_operators: BTreeMap::from_iter([]),
            },
        ),
        (
            "Actor_Name".into(),
            models::ScalarType {
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::new(),
            },
        ),
    ]);
    // ANCHOR_END: schema_scalar_types
    // ANCHOR: schema_object_type_actor
    let actor_type = models::ObjectType {
        description: Some("An actor".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                models::ObjectField {
                    description: Some("The actor's primary key".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "name".into(),
                models::ObjectField {
                    description: Some("The actor's name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "movie_id".into(),
                models::ObjectField {
                    description: Some("The actor's movie ID".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
        ]),
    };
    // ANCHOR_END: schema_object_type_actor
    // ANCHOR: schema_object_type_movie
    let movie_type = models::ObjectType {
        description: Some("A movie".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                models::ObjectField {
                    description: Some("The movie's primary key".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "title".into(),
                models::ObjectField {
                    description: Some("The movie's title".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "rating".into(),
                models::ObjectField {
                    description: Some("The movie's rating".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
        ]),
    };
    // ANCHOR_END: schema_object_type_movie
    // ANCHOR: schema_object_type_namequery
    let name_query_type = models::ObjectType {
        description: Some("parameters for querying by name".into()),
        fields: BTreeMap::from_iter([
            (
                "first_name".into(),
                models::ObjectField {
                    description: Some(
                        "The actor's first name or null to match any first name".into(),
                    ),
                    r#type: models::Type::Nullable {
                        underlying_type: models::Type::Named {
                            name: "String".into(),
                        }
                        .into(),
                    },
                },
            ),
            (
                "last_name".into(),
                models::ObjectField {
                    description: Some("The actor's last name or null to match any last".into()),
                    r#type: models::Type::Nullable {
                        underlying_type: models::Type::Named {
                            name: "String".into(),
                        }
                        .into(),
                    },
                },
            ),
        ]),
    };
    // ANCHOR_END: schema_object_type_namequery
    // ANCHOR: schema_object_type_institution
    let institution_type = models::ObjectType {
        description: Some("An institution".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                models::ObjectField {
                    description: Some("The institution's primary key".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "name".into(),
                models::ObjectField {
                    description: Some("The institution's name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "location".into(),
                models::ObjectField {
                    description: Some("The institution's location".into()),
                    r#type: models::Type::Named {
                        name: "location".into(),
                    },
                },
            ),
            (
                "staff".into(),
                models::ObjectField {
                    description: Some("The institution's staff".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "staff_member".into(),
                        }),
                    },
                },
            ),
            (
                "departments".into(),
                models::ObjectField {
                    description: Some("The institution's departments".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                },
            ),
        ]),
    };
    // ANCHOR_END: schema_object_type_institution
    // ANCHOR: schema_object_type_location
    let location_type = models::ObjectType {
        description: Some("A location".into()),
        fields: BTreeMap::from_iter([
            (
                "city".into(),
                models::ObjectField {
                    description: Some("The location's city".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "country".into(),
                models::ObjectField {
                    description: Some("The location's country".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "campuses".into(),
                models::ObjectField {
                    description: Some("The location's campuses".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                },
            ),
        ]),
    };
    // ANCHOR_END: schema_object_type_location
    // ANCHOR: schema_object_type_staff_member
    let staff_member_type = models::ObjectType {
        description: Some("A staff member".into()),
        fields: BTreeMap::from_iter([
            (
                "first_name".into(),
                models::ObjectField {
                    description: Some("The staff member's first name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "last_name".into(),
                models::ObjectField {
                    description: Some("The staff member's last name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "specialities".into(),
                models::ObjectField {
                    description: Some("The staff member's specialities".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                },
            ),
        ]),
    };
    // ANCHOR_END: schema_object_type_staff_member
    // ANCHOR: schema_object_types
    let object_types = BTreeMap::from_iter([
        ("actor".into(), actor_type),
        ("movie".into(), movie_type),
        ("name_query".into(), name_query_type),
        ("institution".into(), institution_type),
        ("location".into(), location_type),
        ("staff_member".into(), staff_member_type),
    ]);
    // ANCHOR_END: schema_object_types
    // ANCHOR: schema_collection_actor
    let actors_collection = models::CollectionInfo {
        name: "actors".into(),
        description: Some("A collection of actors".into()),
        collection_type: "actor".into(),
        arguments: BTreeMap::new(),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "ActorByID".into(),
            models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
    };
    // ANCHOR_END: schema_collection_actor
    // ANCHOR: schema_collection_movie
    let movies_collection = models::CollectionInfo {
        name: "movies".into(),
        description: Some("A collection of movies".into()),
        collection_type: "movie".into(),
        arguments: BTreeMap::new(),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "MovieByID".into(),
            models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
    };
    // ANCHOR_END: schema_collection_movie
    // ANCHOR: schema_collection_institution
    let institutions_collection = models::CollectionInfo {
        name: "institutions".into(),
        description: Some("A collection of institutions".into()),
        collection_type: "institution".into(),
        arguments: BTreeMap::new(),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "InstitutionByID".into(),
            models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
    };
    // ANCHOR_END: schema_collection_institution
    // ANCHOR: schema_collection_actors_by_movie
    let actors_by_movie_collection = models::CollectionInfo {
        name: "actors_by_movie".into(),
        description: Some("Actors parameterized by movie".into()),
        collection_type: "actor".into(),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            models::ArgumentInfo {
                argument_type: models::Type::Named { name: "Int".into() },
                description: None,
            },
        )]),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::new(),
    };
    // ANCHOR_END: schema_collection_actors_by_movie
    // ANCHOR: movies_by_actor_name_collection
    let movies_by_actor_name_collection = models::CollectionInfo {
        name: "movies_by_actor_name".into(),
        description: Some("Movies filtered by actor name search parameters".into()),
        collection_type: "movie".into(),
        arguments: BTreeMap::from_iter([(
            "actor_name".into(),
            models::ArgumentInfo {
                description: Some("the actor name components to search by".into()),
                argument_type: models::Type::Named {
                    name: "name_query".into(),
                },
            },
        )]),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::new(),
    };
    // ANCHOR_END: movies_by_actor_name_collection
    // ANCHOR: schema_collections
    let collections = vec![
        actors_collection,
        movies_collection,
        institutions_collection,
        actors_by_movie_collection,
        movies_by_actor_name_collection,
    ];
    // ANCHOR_END: schema_collections

    // ANCHOR: schema_procedure_upsert_actor
    let upsert_actor = models::ProcedureInfo {
        name: "upsert_actor".into(),
        description: Some("Insert or update an actor".into()),
        arguments: BTreeMap::from_iter([(
            "actor".into(),
            models::ArgumentInfo {
                description: Some("The actor to insert or update".into()),
                argument_type: models::Type::Named {
                    name: "actor".into(),
                },
            },
        )]),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
    };
    // ANCHOR_END: schema_procedure_upsert_actor

    // ANCHOR: schema_procedure_update_actor_title_by_id
    let update_actor_name_by_id = models::ProcedureInfo {
        name: "update_actor_name_by_id".into(),
        description: Some("Update an actor name given the ID and new name".into()),
        arguments: BTreeMap::from_iter([
            (
                "id".into(),
                models::ArgumentInfo {
                    description: Some("the id of the actor to update".into()),
                    argument_type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "name".into(),
                models::ArgumentInfo {
                    description: Some("the new name of the actor".into()),
                    argument_type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
        ]),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
    };
    // ANCHOR_END: schema_procedure_update_actor_title_by_id

    // ANCHOR: schema_procedure_noop_procedure
    let noop_procedure = models::ProcedureInfo {
        name: "noop_procedure".into(),
        description: Some(
            "Procedure which does not perform any actual mutuations on the data".into(),
        ),
        arguments: BTreeMap::new(),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "String".into(),
            }),
        },
    };
    // ANCHOR_END: schema_procedure_noop_procedure

    // ANCHOR: schema_procedures
    let procedures = vec![upsert_actor, update_actor_name_by_id, noop_procedure];
    // ANCHOR_END: schema_procedures

    // ANCHOR: schema_function_latest_actor_id
    let latest_actor_id_function = models::FunctionInfo {
        name: "latest_actor_id".into(),
        description: Some("Get the ID of the most recent actor".into()),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named { name: "Int".into() }),
        },
        arguments: BTreeMap::new(),
    };
    // ANCHOR_END: schema_function_latest_actor_id

    // ANCHOR: schema_function_latest_actor_name
    // Returns the name of the most recent actor as a custom scalar type
    let latest_actor_name_function = models::FunctionInfo {
        name: "latest_actor_name".into(),
        description: Some("Get the name of the most recent actor".into()),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "Actor_Name".into(),
            }),
        },
        arguments: BTreeMap::new(),
    };
    // ANCHOR_END: schema_function_latest_actor_name

    // ANCHOR: schema_function_latest_actor
    let latest_actor_function = models::FunctionInfo {
        name: "latest_actor".into(),
        description: Some("Get the most recent actor".into()),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
        arguments: BTreeMap::new(),
    };
    // ANCHOR_END: schema_function_latest_actor

    // ANCHOR: schema_function_get_actor_by_id
    let get_actor_by_id_function = models::FunctionInfo {
        name: "get_actor_by_id".into(),
        description: Some("Get actor by ID".into()),
        arguments: BTreeMap::from_iter([(
            "id".into(),
            models::ArgumentInfo {
                description: Some("the id of the actor to fetch".into()),
                argument_type: models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
    };
    // ANCHOR_END: schema_function_get_actor_by_id

    // ANCHOR: schema_function_get_movie_by_id
    let get_movie_by_id_function = models::FunctionInfo {
        name: "get_movie_by_id".into(),
        description: Some("Get movie by ID".into()),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            models::ArgumentInfo {
                description: Some("the id of the movie to fetch".into()),
                argument_type: models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "movie".into(),
            }),
        },
    };
    // ANCHOR_END: schema_function_get_movie_by_id

    // ANCHOR: schema_function_get_actors_by_name
    let get_actors_by_name_function = models::FunctionInfo {
        name: "get_actors_by_name".into(),
        description: Some("Get actors by name".into()),
        arguments: BTreeMap::from_iter([(
            "name".into(),
            models::ArgumentInfo {
                description: Some("the name components to search by".into()),
                argument_type: models::Type::Named {
                    name: "name_query".into(),
                },
            },
        )]),
        result_type: models::Type::Array {
            element_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
    };
    // ANCHOR_END: schema_function_get_actors_by_name

    // ANCHOR: schema_function_get_actors_by_movie_id
    let get_actors_by_movie_id_function = models::FunctionInfo {
        name: "get_actors_by_movie_id".into(),
        description: Some("Get all actors from a movie by movie ID".into()),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            models::ArgumentInfo {
                description: Some("the id of the movie to fetch the actors from".into()),
                argument_type: models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: models::Type::Array {
            element_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
    };
    // ANCHOR_END: schema_function_get_actors_by_movie_id

    // ANCHOR: schema_function_get_all_actors
    let get_all_actors_function = models::FunctionInfo {
        name: "get_all_actors".into(),
        description: Some("Get all the actors".into()),
        result_type: models::Type::Array {
            element_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
        arguments: BTreeMap::new(),
    };
    // ANCHOR_END: schema_function_get_all_actors

    // ANCHOR: schema_function_get_all_movies
    let get_all_movies_function = models::FunctionInfo {
        name: "get_all_movies".into(),
        description: Some("Get all the movies".into()),
        result_type: models::Type::Array {
            element_type: Box::new(models::Type::Named {
                name: "movie".into(),
            }),
        },
        arguments: BTreeMap::new(),
    };
    // ANCHOR_END: schema_function_get_all_movies

    // ANCHOR: schema_functions
    let functions: Vec<models::FunctionInfo> = vec![
        latest_actor_id_function,
        latest_actor_name_function,
        latest_actor_function,
        get_actor_by_id_function,
        get_movie_by_id_function,
        get_actors_by_name_function,
        get_actors_by_movie_id_function,
        get_all_actors_function,
        get_all_movies_function,
    ];
    // ANCHOR_END: schema_functions
    // ANCHOR: schema2
    Json(models::SchemaResponse {
        scalar_types,
        object_types,
        collections,
        functions,
        procedures,
    })
}
// ANCHOR_END: schema2
// ANCHOR: post_query
// ANCHOR: post_query_signature
pub async fn post_query(
    State(state): State<Arc<Mutex<AppState>>>,
    Json(request): Json<models::QueryRequest>,
) -> Result<Json<models::QueryResponse>> {
    // ANCHOR_END: post_query_signature
    let state = state.lock().await;

    let variable_sets = request.variables.unwrap_or(vec![BTreeMap::new()]);

    let mut row_sets = vec![];
    for variables in variable_sets.iter() {
        let row_set = execute_query_with_variables(
            &request.collection,
            &request.arguments,
            &request.collection_relationships,
            &request.query,
            variables,
            &state,
        )?;
        row_sets.push(row_set);
    }

    Ok(Json(models::QueryResponse(row_sets)))
}
// ANCHOR_END: post_query
// ANCHOR: execute_query_with_variables
// ANCHOR: execute_query_with_variables_signature
fn execute_query_with_variables(
    collection: &str,
    arguments: &BTreeMap<String, models::Argument>,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    query: &models::Query,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<models::RowSet> {
    // ANCHOR_END: execute_query_with_variables_signature
    let mut argument_values = BTreeMap::new();

    for (argument_name, argument_value) in arguments.iter() {
        if argument_values
            .insert(
                argument_name.clone(),
                eval_argument(variables, argument_value)?,
            )
            .is_some()
        {
            return Err((
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "duplicate argument names".into(),
                    details: serde_json::Value::Null,
                }),
            ));
        }
    }

    let collection = get_collection_by_name(
        collection_relationships,
        variables,
        collection,
        &argument_values,
        state,
        query,
    )?;

    execute_query(
        collection_relationships,
        variables,
        state,
        query,
        Root::CurrentRow,
        collection,
    )
}
// ANCHOR_END: execute_query_with_variables
// ANCHOR: get_collection_by_name
fn get_collection_by_name(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    collection_name: &str,
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    query: &models::Query,
) -> Result<Vec<Row>> {
    match collection_name {
        "actors" => Ok(state.actors.values().cloned().collect()),
        "movies" => Ok(state.movies.values().cloned().collect()),
        "institutions" => Ok(state.institutions.values().cloned().collect()),
        "actors_by_movie" => actors_by_movie_rows(arguments, state),
        "movies_by_actor_name" => movies_by_actor_name_rows(arguments, state),
        "latest_actor_id" => latest_actor_id_rows(state),
        "latest_actor_name" => latest_actor_name_rows(state),
        "latest_actor" => latest_actor_rows(state, query, collection_relationships, variables),
        "get_actor_by_id" => {
            get_actor_by_id_rows(arguments, state, query, collection_relationships, variables)
        }
        "get_movie_by_id" => {
            get_movie_by_id_rows(arguments, state, query, collection_relationships, variables)
        }
        "get_actors_by_name" => {
            get_actors_by_name_rows(arguments, state, query, collection_relationships, variables)
        }
        "actor_names_by_movie" => actor_names_by_movie_rows(arguments, state),
        "get_all_actors" => get_all_actors_rows(state, query, collection_relationships, variables),
        "get_all_movies" => get_all_movies_rows(state, query, collection_relationships, variables),
        "get_actors_by_movie_id_bounds" => get_actors_by_movie_id_bounds_rows(
            arguments,
            state,
            query,
            collection_relationships,
            variables,
        ),
        "get_actors_by_movie_id" => {
            get_actors_by_movie_rows(arguments, state, query, collection_relationships, variables)
        }
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(models::ErrorResponse {
                message: "invalid collection name".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}
// ANCHOR_END: get_collection_by_name

fn actors_by_movie_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let movie_id = arguments.get("movie_id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "missing argument movie_id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let movie_id_int = movie_id.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "movie_id must be a integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let mut actors_by_movie = vec![];

    for (_id, actor) in state.actors.iter() {
        let actor_movie_id_int = get_actor_movie_id(actor)?;
        if actor_movie_id_int == movie_id_int {
            actors_by_movie.push(actor.clone())
        }
    }

    Ok(actors_by_movie)
}

fn get_actor_movie_id(actor: &BTreeMap<String, serde_json::Value>) -> Result<i64> {
    let actor_movie_id = actor.get("movie_id").ok_or((
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(models::ErrorResponse {
            message: "actor movie_id not found".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let actor_movie_id_int = actor_movie_id.as_i64().ok_or((
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(models::ErrorResponse {
            message: "actor movie_id is not an integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    Ok(actor_movie_id_int)
}

fn movies_by_actor_name_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let name_object = parse_object_argument("actor_name", arguments)?;
    let (filter_first_name, filter_last_name) = parse_name_query_object(name_object)?;

    let movie_ids = filter_actors_by_name(state, filter_first_name, filter_last_name)
        .map(|result| result.and_then(get_actor_movie_id))
        .collect::<Result<HashSet<i64>>>()?;

    let movies_by_actor = movie_ids
        .iter()
        .filter_map(|movie_id| state.movies.get(movie_id).cloned())
        .collect::<Vec<_>>();

    Ok(movies_by_actor)
}

fn latest_actor_id_rows(state: &AppState) -> Result<Vec<Row>> {
    let latest_id = state
        .actors
        .iter()
        .filter_map(|(_id, a)| a.get("id").and_then(|v| v.as_i64()))
        .max();
    let latest_id_value = serde_json::to_value(latest_id).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        latest_id_value,
    )])])
}

fn latest_actor_name_rows(state: &AppState) -> Result<Vec<Row>> {
    let latest_name = state
        .actors
        .iter()
        .filter_map(|(_id, a)| a.get("name").and_then(|v| v.as_str()))
        .max();
    let latest_name_value = serde_json::to_value(latest_name).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        latest_name_value,
    )])])
}

fn latest_actor_rows(
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let latest_id = state
        .actors
        .iter()
        .filter_map(|(_id, a)| a.get("id").and_then(|v| v.as_i64()))
        .max();

    if let Some(id) = latest_id {
        let latest_actor = state.actors.get(&id);

        let rows = latest_actor
            .map(|actor| project_row(actor, state, query, collection_relationships, variables))
            .transpose()?
            .flatten();

        let latest_actor_value = serde_json::to_value(rows).map_err(|_| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: "unable to encode value".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

        Ok(vec![BTreeMap::from_iter([(
            "__value".into(),
            latest_actor_value,
        )])])
    } else {
        Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "No max ID exists".into(),
                details: serde_json::Value::Null,
            }),
        ))
    }
}

fn get_actor_by_id_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let id_value = arguments.get("id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "missing argument id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    if let Some(id) = id_value.as_i64() {
        let actor = state.actors.get(&id);

        match actor {
            None => Ok(vec![BTreeMap::from_iter([(
                "__value".into(),
                serde_json::Value::Null,
            )])]),
            Some(actor) => {
                let rows = project_row(actor, state, query, collection_relationships, variables)?;

                let actor_value = serde_json::to_value(rows).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "unable to encode value".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;

                Ok(vec![BTreeMap::from_iter([("__value".into(), actor_value)])])
            }
        }
    } else {
        Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "incorrect type for id".into(),
                details: serde_json::Value::Null,
            }),
        ))
    }
}

fn get_movie_by_id_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let id_value = arguments.get("movie_id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "missing argument movie_id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    if let Some(id) = id_value.as_i64() {
        let movie = state.movies.get(&id);

        match movie {
            None => Ok(vec![BTreeMap::from_iter([(
                "__value".into(),
                serde_json::Value::Null,
            )])]),
            Some(movie) => {
                let rows = project_row(movie, state, query, collection_relationships, variables)?;

                let movie_value = serde_json::to_value(rows).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "unable to encode value".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;

                Ok(vec![BTreeMap::from_iter([("__value".into(), movie_value)])])
            }
        }
    } else {
        Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "incorrect type for id".into(),
                details: serde_json::Value::Null,
            }),
        ))
    }
}

fn get_actors_by_movie_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let movie_id = arguments.get("movie_id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "missing argument movie_id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let movie_id_int = movie_id.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "movie_id must be a integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let mut actors_by_movie = vec![];

    for (_id, actor) in state.actors.iter() {
        let actor_movie_id_int = get_actor_movie_id(actor)?;

        if actor_movie_id_int == movie_id_int {
            let row = project_row(actor, state, query, collection_relationships, variables)?;
            actors_by_movie.push(row)
        }
    }

    let actors_by_movie_value = serde_json::to_value(actors_by_movie).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actors_by_movie_value,
    )])])
}

fn project_row(
    row: &Row,
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Option<IndexMap<String, models::RowFieldValue>>> {
    query
        .fields
        .as_ref()
        .map(|fields| {
            fields
                .iter()
                .map(|(field_name, field)| {
                    let field_value =
                        eval_field(collection_relationships, variables, state, field, row)?;
                    Ok((field_name.clone(), field_value))
                })
                .collect::<Result<IndexMap<String, models::RowFieldValue>>>()
        })
        .transpose()
}

fn get_actors_by_name_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let name_object = parse_object_argument("name", arguments)?;
    let (filter_first_name, filter_last_name) = parse_name_query_object(name_object)?;

    let filtered_actors = filter_actors_by_name(state, filter_first_name, filter_last_name)
        .filter_map(|result| match result {
            Ok(actor) => {
                match project_row(actor, state, query, collection_relationships, variables) {
                    Ok(Some(row)) => Some(Ok(row)),
                    Ok(None) => None,
                    Err(err) => Some(Err(err)),
                }
            }
            Err(err) => Some(Err(err)),
        })
        .collect::<Result<Vec<IndexMap<String, models::RowFieldValue>>>>()?;

    let actors_value = serde_json::to_value(filtered_actors).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "unable to encode value".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actors_value,
    )])])
}

fn parse_object_argument<'a>(
    argument_name: &str,
    arguments: &'a BTreeMap<String, serde_json::Value>,
) -> Result<&'a serde_json::Map<String, serde_json::Value>> {
    let name_object = arguments
        .get(argument_name)
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "missing argument name".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .as_object()
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "name must be an object".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    Ok(name_object)
}

fn parse_name_query_object(
    name_object: &serde_json::Map<String, serde_json::Value>,
) -> Result<(Option<&str>, Option<&str>)> {
    let first_name_value = name_object.get("first_name").ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(models::ErrorResponse {
                message: "missing name argument property first_name".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    let first_name = if first_name_value.is_null() {
        None
    } else {
        Some(first_name_value.as_str().ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "name argument property first_name must be a string".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?)
    };
    let last_name_value = name_object.get("last_name").ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(models::ErrorResponse {
                message: "missing name argument property last_name".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    let last_name = if last_name_value.is_null() {
        None
    } else {
        Some(last_name_value.as_str().ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "name argument property last_name must be a string".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?)
    };
    Ok((first_name, last_name))
}

fn filter_actors_by_name<'a>(
    state: &'a AppState,
    filter_first_name: Option<&'a str>,
    filter_last_name: Option<&'a str>,
) -> impl std::iter::Iterator<Item = Result<&'a BTreeMap<String, serde_json::Value>>> {
    state
        .actors
        .values()
        .map(|actor| {
            let actor_name = actor
                .get("name")
                .ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(models::ErrorResponse {
                            message: "actor missing name".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?
                .as_str()
                .ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(models::ErrorResponse {
                            message: "actor name must be a string".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;
            let names = actor_name.split(' ').collect::<Vec<_>>();
            let actor_first_name = names
                .first()
                .map(|str| str.to_string())
                .unwrap_or(String::new());
            let actor_last_name = names.into_iter().skip(1).collect::<Vec<_>>().join(" ");

            Ok((actor_first_name, actor_last_name, actor))
        })
        .filter_map(move |result| match result {
            Ok((actor_first_name, actor_last_name, actor)) => {
                if filter_first_name
                    .map(|first_name| first_name == actor_first_name)
                    .unwrap_or(true)
                    && filter_last_name
                        .map(|last_name| last_name == actor_last_name)
                        .unwrap_or(true)
                {
                    Some(Ok(actor))
                } else {
                    None
                }
            }
            Err(err) => Some(Err(err)),
        })
}

fn actor_names_by_movie_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let movie_id = arguments.get("movie_id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "missing argument movie_id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let movie_id_int = movie_id.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "movie_id must be a string".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let mut actor_names_by_movie = vec![];

    for (_id, actor) in state.actors.iter() {
        let actor_movie_id = actor.get("movie_id").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "movie_id not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_movie_id_int = actor_movie_id.as_i64().ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_name = actor.get("name").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "name not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        if actor_movie_id_int == movie_id_int {
            actor_names_by_movie.push(actor_name.clone())
        }
    }
    let actor_names_by_movie_value = serde_json::to_value(actor_names_by_movie).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actor_names_by_movie_value,
    )])])
}

fn get_all_actors_rows(
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let mut actors = vec![];
    for (_id, actor) in state.actors.iter() {
        let rows = project_row(actor, state, query, collection_relationships, variables)?;
        let actor_value = serde_json::to_value(rows).map_err(|_| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: "unable to encode value".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
        actors.push(actor_value);
    }
    let actors_value = serde_json::to_value(actors).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "unable to encode value".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actors_value,
    )])])
}

fn get_all_movies_rows(
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let mut movies = vec![];
    for (_id, movie) in state.movies.iter() {
        let rows = project_row(movie, state, query, collection_relationships, variables)?;
        let movie_value = serde_json::to_value(rows).map_err(|_| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: "unable to encode value".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
        movies.push(movie_value);
    }
    let movies_value = serde_json::to_value(movies).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "unable to encode value".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        movies_value,
    )])])
}

fn get_actors_by_movie_id_bounds_rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    query: &models::Query,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<Vec<Row>> {
    let lower_bound_value = arguments.get("lower_bound").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "missing argument movie id lower bound".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let lower_bound = lower_bound_value.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "movie id bound must be an integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let upper_bound_value = arguments.get("upper_bound").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "missing argument movie id upper bound".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let upper_bound = upper_bound_value.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "movie id bound must be an integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let mut actors_by_movie_id_bounds = vec![];

    for (_id, actor) in state.actors.iter() {
        let movie_id = actor.get("movie_id").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "movie id not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let movie_id_int = movie_id.as_i64().ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "movie id not an integer".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        if movie_id_int >= lower_bound && movie_id_int <= upper_bound {
            let rows = project_row(actor, state, query, collection_relationships, variables)?;
            let actors_value = serde_json::to_value(rows).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(models::ErrorResponse {
                        message: "unable to encode value".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            actors_by_movie_id_bounds.push(actors_value);
        }
    }

    let actors_by_movie_id_bounds_value =
        serde_json::to_value(actors_by_movie_id_bounds).map_err(|_| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: "unable to encode value".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actors_by_movie_id_bounds_value,
    )])])
}

/// ANCHOR: Root
enum Root<'a> {
    /// References to the root collection actually
    /// refer to the current row, because the path to
    /// the nearest enclosing [`models::Query`] does not pass
    /// an [`models::Expression::Exists`] node.
    CurrentRow,
    /// References to the root collection refer to the
    /// explicitly-identified row, which is the row
    /// being evaluated in the context of the nearest enclosing
    /// [`models::Query`].
    ExplicitRow(&'a Row),
}
/// ANCHOR_END: Root
// ANCHOR: execute_query
// ANCHOR: execute_query_signature
fn execute_query(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    query: &models::Query,
    root: Root,
    collection: Vec<Row>,
) -> Result<models::RowSet> {
    // ANCHOR_END: execute_query_signature
    // ANCHOR: execute_query_sort
    let sorted = sort(
        collection_relationships,
        variables,
        state,
        collection,
        &query.order_by,
    )?;
    // ANCHOR_END: execute_query_sort
    // ANCHOR: execute_query_filter
    let filtered: Vec<Row> = (match &query.predicate {
        None => Ok(sorted),
        Some(expr) => {
            let mut filtered: Vec<Row> = vec![];
            for item in sorted.into_iter() {
                let root = match root {
                    Root::CurrentRow => &item,
                    Root::ExplicitRow(root) => root,
                };
                if eval_expression(
                    collection_relationships,
                    variables,
                    state,
                    expr,
                    root,
                    &item,
                )? {
                    filtered.push(item);
                }
            }
            Ok(filtered)
        }
    })?;
    // ANCHOR_END: execute_query_filter
    // ANCHOR: execute_query_paginate
    let paginated: Vec<Row> = paginate(filtered.into_iter(), query.limit, query.offset);

    // ANCHOR_END: execute_query_paginate
    // ANCHOR: execute_query_aggregates
    let aggregates = query
        .aggregates
        .as_ref()
        .map(|aggregates| {
            let mut row: IndexMap<String, serde_json::Value> = IndexMap::new();
            for (aggregate_name, aggregate) in aggregates.iter() {
                row.insert(
                    aggregate_name.clone(),
                    eval_aggregate(aggregate, &paginated)?,
                );
            }
            Ok(row)
        })
        .transpose()?;
    // ANCHOR_END: execute_query_aggregates
    // ANCHOR: execute_query_fields
    let rows = query
        .fields
        .as_ref()
        .map(|fields| {
            let mut rows: Vec<IndexMap<String, models::RowFieldValue>> = vec![];
            for item in paginated.iter() {
                // If item contains the key __value, then we are dealing with the response
                // from a function, and we need to handle it differently.
                if item.contains_key("__value") {
                    let value_field = models::Field::Column {
                        column: String::from("__value"),
                        fields: None,
                    };
                    let mut row = IndexMap::new();
                    row.insert(
                        "__value".into(),
                        eval_field(
                            collection_relationships,
                            variables,
                            state,
                            &value_field,
                            item,
                        )?,
                    );
                    rows.push(row);
                } else {
                    let row = eval_row(fields, collection_relationships, variables, state, item)?;
                    rows.push(row)
                }
            }
            Ok(rows)
        })
        .transpose()?;
    // ANCHOR_END: execute_query_fields
    // ANCHOR: execute_query_rowset

    Ok(models::RowSet { aggregates, rows })
    // ANCHOR_END: execute_query_rowset
}
// ANCHOR_END: execute_query
// ANCHOR: eval_row
fn eval_row(
    fields: &IndexMap<String, models::Field>,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, Value>,
    state: &AppState,
    item: &BTreeMap<String, Value>,
) -> Result<IndexMap<String, models::RowFieldValue>> {
    let mut row = IndexMap::new();
    for (field_name, field) in fields.iter() {
        row.insert(
            field_name.clone(),
            eval_field(collection_relationships, variables, state, field, item)?,
        );
    }
    Ok(row)
}
// ANCHOR_END: eval_row
// ANCHOR: eval_aggregate
fn eval_aggregate(
    aggregate: &models::Aggregate,
    paginated: &Vec<BTreeMap<String, serde_json::Value>>,
) -> Result<serde_json::Value> {
    match aggregate {
        models::Aggregate::StarCount {} => Ok(serde_json::Value::from(paginated.len())),
        models::Aggregate::ColumnCount { column, distinct } => {
            let values = paginated
                .iter()
                .map(|row| {
                    row.get(column).ok_or((
                        StatusCode::BAD_REQUEST,
                        Json(models::ErrorResponse {
                            message: "invalid column name".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))
                })
                .collect::<Result<Vec<_>>>()?;

            let non_null_values = values.iter().filter(|value| !value.is_null());

            let agg_value = if *distinct {
                non_null_values
                    .map(|value| {
                        serde_json::to_string(value).map_err(|_| {
                            (
                                StatusCode::INTERNAL_SERVER_ERROR,
                                Json(models::ErrorResponse {
                                    message: "unable to encode value".into(),
                                    details: serde_json::Value::Null,
                                }),
                            )
                        })
                    })
                    .collect::<Result<HashSet<_>>>()?
                    .len()
            } else {
                non_null_values.count()
            };
            serde_json::to_value(agg_value).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(models::ErrorResponse {
                        message: " ".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })
        }
        models::Aggregate::SingleColumn { column, function } => {
            let values = paginated
                .iter()
                .map(|row| {
                    row.get(column).ok_or((
                        StatusCode::BAD_REQUEST,
                        Json(models::ErrorResponse {
                            message: "invalid column name".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))
                })
                .collect::<Result<Vec<_>>>()?;
            eval_aggregate_function(function, values)
        }
    }
}
// ANCHOR_END: eval_aggregate
// ANCHOR: eval_aggregate_function
fn eval_aggregate_function(
    function: &str,
    values: Vec<&serde_json::Value>,
) -> Result<serde_json::Value> {
    let int_values = values
        .iter()
        .map(|value| {
            value.as_i64().ok_or((
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "column is not an integer".into(),
                    details: serde_json::Value::Null,
                }),
            ))
        })
        .collect::<Result<Vec<_>>>()?;
    let agg_value = match function {
        "min" => Ok(int_values.iter().min()),
        "max" => Ok(int_values.iter().max()),
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(models::ErrorResponse {
                message: "invalid aggregation function".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }?;
    serde_json::to_value(agg_value).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })
}
// ANCHOR_END: eval_aggregate_function
// ANCHOR: sort
fn sort(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    collection: Vec<Row>,
    order_by: &Option<models::OrderBy>,
) -> Result<Vec<Row>> {
    match order_by {
        None => Ok(collection),
        Some(order_by) => {
            let mut copy = vec![];
            for item_to_insert in collection.into_iter() {
                let mut index = 0;
                for other in copy.iter() {
                    if let Ordering::Greater = eval_order_by(
                        collection_relationships,
                        variables,
                        state,
                        order_by,
                        other,
                        &item_to_insert,
                    )? {
                        break;
                    } else {
                        index += 1;
                    }
                }
                copy.insert(index, item_to_insert);
            }
            Ok(copy)
        }
    }
}
// ANCHOR_END: sort
// ANCHOR: paginate
fn paginate<I: Iterator<Item = Row>>(
    collection: I,
    limit: Option<u32>,
    offset: Option<u32>,
) -> Vec<Row> {
    let start = offset.unwrap_or(0).try_into().unwrap();
    match limit {
        Some(n) => collection.skip(start).take(n.try_into().unwrap()).collect(),
        None => collection.skip(start).collect(),
    }
}
// ANCHOR_END: paginate
// ANCHOR: eval_order_by
fn eval_order_by(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    order_by: &models::OrderBy,
    t1: &Row,
    t2: &Row,
) -> Result<Ordering> {
    let mut result = Ordering::Equal;

    for element in order_by.elements.iter() {
        let v1 = eval_order_by_element(collection_relationships, variables, state, element, t1)?;
        let v2 = eval_order_by_element(collection_relationships, variables, state, element, t2)?;
        let x = match element.order_direction {
            models::OrderDirection::Asc => compare(v1, v2)?,
            models::OrderDirection::Desc => compare(v2, v1)?,
        };
        result = result.then(x);
    }

    Ok(result)
}
// ANCHOR_END: eval_order_by
// ANCHOR: compare
fn compare(v1: serde_json::Value, v2: serde_json::Value) -> Result<Ordering> {
    match (v1, v2) {
        (serde_json::Value::Null, serde_json::Value::Null) => Ok(Ordering::Equal),
        (serde_json::Value::Null, _) => Ok(Ordering::Less),
        (_, serde_json::Value::Null) => Ok(Ordering::Greater),

        (serde_json::Value::Bool(b1), serde_json::Value::Bool(b2)) => Ok(b1.cmp(&b2)),
        (serde_json::Value::Number(n1), serde_json::Value::Number(n2)) => {
            if n1.as_f64().unwrap() < n2.as_f64().unwrap() {
                Ok(Ordering::Less)
            } else {
                Ok(Ordering::Greater)
            }
        }
        (serde_json::Value::String(s1), serde_json::Value::String(s2)) => Ok(s1.cmp(&s2)),
        _ => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "cannot compare values".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}
// ANCHOR_END: compare
// ANCHOR: eval_order_by_element
fn eval_order_by_element(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    element: &models::OrderByElement,
    item: &Row,
) -> Result<serde_json::Value> {
    match element.target.clone() {
        models::OrderByTarget::Column { name, path } => {
            eval_order_by_column(collection_relationships, variables, state, item, path, name)
        }
        models::OrderByTarget::SingleColumnAggregate {
            column,
            function,
            path,
        } => eval_order_by_single_column_aggregate(
            collection_relationships,
            variables,
            state,
            item,
            path,
            column,
            function,
        ),
        models::OrderByTarget::StarCountAggregate { path } => eval_order_by_star_count_aggregate(
            collection_relationships,
            variables,
            state,
            item,
            path,
        ),
    }
}
// ANCHOR_END: eval_order_by_element
// ANCHOR: eval_order_by_star_count_aggregate
fn eval_order_by_star_count_aggregate(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    item: &BTreeMap<String, serde_json::Value>,
    path: Vec<models::PathElement>,
) -> Result<serde_json::Value> {
    let rows: Vec<Row> = eval_path(collection_relationships, variables, state, &path, item)?;
    Ok(rows.len().into())
}
// ANCHOR_END: eval_order_by_star_count_aggregate
// ANCHOR: eval_order_by_single_column_aggregate
fn eval_order_by_single_column_aggregate(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    item: &BTreeMap<String, serde_json::Value>,
    path: Vec<models::PathElement>,
    column: String,
    function: String,
) -> Result<serde_json::Value> {
    let rows: Vec<Row> = eval_path(collection_relationships, variables, state, &path, item)?;
    let values = rows
        .iter()
        .map(|row| {
            row.get(column.as_str()).ok_or((
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "invalid column name".into(),
                    details: serde_json::Value::Null,
                }),
            ))
        })
        .collect::<Result<Vec<_>>>()?;
    eval_aggregate_function(&function, values)
}
// ANCHOR_END: eval_order_by_single_column_aggregate
// ANCHOR: eval_order_by_column
fn eval_order_by_column(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    item: &BTreeMap<String, serde_json::Value>,
    path: Vec<models::PathElement>,
    name: String,
) -> Result<serde_json::Value> {
    let rows: Vec<Row> = eval_path(collection_relationships, variables, state, &path, item)?;
    if rows.len() > 1 {
        return Err((
            StatusCode::BAD_REQUEST,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        ));
    }
    match rows.first() {
        Some(row) => eval_column(row, name.as_str()),
        None => Ok(serde_json::Value::Null),
    }
}
// ANCHOR_END: eval_order_by_column
// ANCHOR: eval_path
fn eval_path(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    path: &[models::PathElement],
    item: &Row,
) -> Result<Vec<Row>> {
    let mut result: Vec<Row> = vec![item.clone()];

    for path_element in path.iter() {
        let relationship_name = path_element.relationship.as_str();
        let relationship = collection_relationships.get(relationship_name).ok_or((
            StatusCode::BAD_REQUEST,
            Json(models::ErrorResponse {
                message: "invalid relationship name in path".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        result = eval_path_element(
            collection_relationships,
            variables,
            state,
            relationship,
            &path_element.arguments,
            &result,
            None,
            &path_element.predicate,
        )?;
    }

    Ok(result)
}
// ANCHOR_END: eval_path
// ANCHOR: eval_path_element
fn eval_path_element(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    relationship: &models::Relationship,
    arguments: &BTreeMap<String, models::RelationshipArgument>,
    source: &[Row],
    query: Option<&Query>,
    predicate: &Option<Box<models::Expression>>,
) -> Result<Vec<Row>> {
    let mut matching_rows: Vec<Row> = vec![];

    // Note: Join strategy
    //
    // Rows can be related in two ways: 1) via a column mapping, and
    // 2) via collection arguments. Because collection arguments can be computed
    // using the columns on the source side of a relationship, in general
    // we need to compute the target collection once for each source row.
    // This join strategy can result in some target rows appearing in the
    // resulting row set more than once, if two source rows are both related
    // to the same target row.
    //
    // In practice, this is not an issue, either because a) the relationship
    // is computed in the course of evaluating a predicate, and all predicates are
    // implicitly or explicitly existentially quantified, or b) if the
    // relationship is computed in the course of evaluating an ordering, the path
    // should consist of all object relationships, and possibly terminated by a
    // single array relationship, so there should be no double counting.

    for src_row in source.iter() {
        let mut all_arguments = BTreeMap::new();

        for (argument_name, argument_value) in relationship.arguments.iter() {
            if all_arguments
                .insert(
                    argument_name.clone(),
                    eval_relationship_argument(variables, src_row, argument_value)?,
                )
                .is_some()
            {
                return Err((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "duplicate argument names".into(),
                        details: serde_json::Value::Null,
                    }),
                ));
            }
        }

        for (argument_name, argument_value) in arguments.iter() {
            if all_arguments
                .insert(
                    argument_name.clone(),
                    eval_relationship_argument(variables, src_row, argument_value)?,
                )
                .is_some()
            {
                return Err((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "duplicate argument names".into(),
                        details: serde_json::Value::Null,
                    }),
                ));
            }
        }

        let query = match query {
            None => models::Query {
                aggregates: None,
                fields: Some(IndexMap::new()),
                limit: None,
                offset: None,
                order_by: None,
                predicate: None,
            },
            Some(query) => query.clone(),
        };

        let target = get_collection_by_name(
            collection_relationships,
            variables,
            relationship.target_collection.as_str(),
            &all_arguments,
            state,
            &query,
        )?;

        for tgt_row in target.iter() {
            if let Some(predicate) = predicate {
                if eval_column_mapping(relationship, src_row, tgt_row)?
                    && eval_expression(
                        collection_relationships,
                        variables,
                        state,
                        predicate,
                        tgt_row,
                        tgt_row,
                    )?
                {
                    matching_rows.push(tgt_row.clone());
                }
            } else if eval_column_mapping(relationship, src_row, tgt_row)? {
                matching_rows.push(tgt_row.clone());
            }
        }
    }

    Ok(matching_rows)
}
// ANCHOR_END: eval_path_element
// ANCHOR: eval_argument
fn eval_argument(
    variables: &BTreeMap<String, serde_json::Value>,
    argument: &models::Argument,
) -> Result<serde_json::Value> {
    match argument {
        models::Argument::Variable { name } => {
            let value = variables
                .get(name.as_str())
                .ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "invalid variable name".into(),
                        details: serde_json::Value::Null,
                    }),
                ))
                .cloned()?;
            Ok(value)
        }
        models::Argument::Literal { value } => Ok(value.clone()),
    }
}
// ANCHOR_END: eval_argument
// ANCHOR: eval_relationship_argument
fn eval_relationship_argument(
    variables: &BTreeMap<String, serde_json::Value>,
    row: &Row,
    argument: &models::RelationshipArgument,
) -> Result<serde_json::Value> {
    match argument {
        models::RelationshipArgument::Variable { name } => {
            let value = variables
                .get(name.as_str())
                .ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "invalid variable name".into(),
                        details: serde_json::Value::Null,
                    }),
                ))
                .cloned()?;
            Ok(value)
        }
        models::RelationshipArgument::Literal { value } => Ok(value.clone()),
        models::RelationshipArgument::Column { name } => eval_column(row, name),
    }
}
// ANCHOR_END: eval_relationship_argument
// ANCHOR: eval_expression
// ANCHOR: eval_expression_signature
fn eval_expression(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    expr: &models::Expression,
    root: &Row,
    item: &Row,
) -> Result<bool> {
    // ANCHOR_END: eval_expression_signature
    // ANCHOR: eval_expression_logical
    match expr {
        models::Expression::And { expressions } => {
            for expr in expressions.iter() {
                if !eval_expression(collection_relationships, variables, state, expr, root, item)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        models::Expression::Or { expressions } => {
            for expr in expressions.iter() {
                if eval_expression(collection_relationships, variables, state, expr, root, item)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        models::Expression::Not { expression } => {
            let b = eval_expression(
                collection_relationships,
                variables,
                state,
                expression,
                root,
                item,
            )?;
            Ok(!b)
        }
        // ANCHOR_END: eval_expression_logical
        // ANCHOR: eval_expression_unary_operators
        models::Expression::UnaryComparisonOperator { column, operator } => match operator {
            models::UnaryComparisonOperator::IsNull => {
                let vals = eval_comparison_target(
                    collection_relationships,
                    variables,
                    state,
                    column,
                    root,
                    item,
                )?;
                Ok(vals.iter().any(|val| val.is_null()))
            }
        },
        // ANCHOR_END: eval_expression_unary_operators
        // ANCHOR: eval_expression_binary_operators
        models::Expression::BinaryComparisonOperator {
            column,
            operator,
            value,
        } => match operator.as_str() {
            "_eq" => {
                let left_vals = eval_comparison_target(
                    collection_relationships,
                    variables,
                    state,
                    column,
                    root,
                    item,
                )?;
                let right_vals = eval_comparison_value(
                    collection_relationships,
                    variables,
                    state,
                    value,
                    root,
                    item,
                )?;
                for left_val in left_vals.iter() {
                    for right_val in right_vals.iter() {
                        if left_val == right_val {
                            return Ok(true);
                        }
                    }
                }

                Ok(false)
            }
            "like" => {
                let column_vals = eval_comparison_target(
                    collection_relationships,
                    variables,
                    state,
                    column,
                    root,
                    item,
                )?;
                let regex_vals = eval_comparison_value(
                    collection_relationships,
                    variables,
                    state,
                    value,
                    root,
                    item,
                )?;
                for column_val in column_vals.iter() {
                    for regex_val in regex_vals.iter() {
                        let column_str = column_val.as_str().ok_or((
                            StatusCode::BAD_REQUEST,
                            Json(models::ErrorResponse {
                                message: "column is not a string".into(),
                                details: serde_json::Value::Null,
                            }),
                        ))?;
                        let regex_str = regex_val.as_str().ok_or((
                            StatusCode::BAD_REQUEST,
                            Json(models::ErrorResponse {
                                message: " ".into(),
                                details: serde_json::Value::Null,
                            }),
                        ))?;
                        let regex = Regex::new(regex_str).map_err(|_| {
                            (
                                StatusCode::BAD_REQUEST,
                                Json(models::ErrorResponse {
                                    message: "invalid regular expression".into(),
                                    details: serde_json::Value::Null,
                                }),
                            )
                        })?;
                        if regex.is_match(column_str) {
                            return Ok(true);
                        }
                    }
                }
                Ok(false)
            }
            _op => Err((
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: format!("operator '{_op}' not supported"),
                    details: serde_json::Value::Null,
                }),
            )),
        },
        models::Expression::Exists {
            in_collection,
            predicate,
        } => {
            let query = models::Query {
                aggregates: None,
                fields: Some(IndexMap::new()),
                limit: None,
                offset: None,
                order_by: None,
                predicate: predicate.clone().map(|exp| *exp),
            };
            let collection = eval_in_collection(
                collection_relationships,
                item,
                variables,
                state,
                in_collection,
            )?;
            let row_set = execute_query(
                collection_relationships,
                variables,
                state,
                &query,
                Root::ExplicitRow(root),
                collection,
            )?;
            let rows: Vec<IndexMap<_, _>> = row_set.rows.ok_or((
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: " ".into(),
                    details: serde_json::Value::Null,
                }),
            ))?;
            Ok(!rows.is_empty())
        }
    }
}
// ANCHOR_END: eval_expression
// ANCHOR: eval_in_collection
fn eval_in_collection(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    item: &BTreeMap<String, serde_json::Value>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    in_collection: &models::ExistsInCollection,
) -> Result<Vec<Row>> {
    match in_collection {
        models::ExistsInCollection::Related {
            relationship,
            arguments,
        } => {
            let relationship = collection_relationships.get(relationship.as_str()).ok_or((
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: " ".into(),
                    details: serde_json::Value::Null,
                }),
            ))?;
            let source = vec![item.clone()];
            eval_path_element(
                collection_relationships,
                variables,
                state,
                relationship,
                arguments,
                &source,
                None,
                &Some(Box::new(models::Expression::And {
                    expressions: vec![],
                })),
            )
        }
        models::ExistsInCollection::Unrelated {
            collection,
            arguments,
        } => {
            let arguments = arguments
                .iter()
                .map(|(k, v)| Ok((k.clone(), eval_relationship_argument(variables, item, v)?)))
                .collect::<Result<BTreeMap<_, _>>>()?;

            let query = models::Query {
                aggregates: None,
                fields: Some(IndexMap::new()),
                limit: None,
                offset: None,
                order_by: None,
                predicate: None,
            };

            get_collection_by_name(
                collection_relationships,
                variables,
                collection.as_str(),
                &arguments,
                state,
                &query,
            )
        }
    }
}
// ANCHOR_END: eval_in_collection
// ANCHOR: eval_comparison_target
fn eval_comparison_target(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    target: &models::ComparisonTarget,
    root: &Row,
    item: &Row,
) -> Result<Vec<serde_json::Value>> {
    match target {
        models::ComparisonTarget::Column { name, path } => {
            let rows = eval_path(collection_relationships, variables, state, path, item)?;
            let mut values = vec![];
            for row in rows.iter() {
                let value = eval_column(row, name.as_str())?;
                values.push(value);
            }
            Ok(values)
        }
        models::ComparisonTarget::RootCollectionColumn { name } => {
            let value = eval_column(root, name.as_str())?;
            Ok(vec![value])
        }
    }
}
// ANCHOR_END: eval_comparison_target
// ANCHOR: eval_column
fn eval_column(row: &Row, column_name: &str) -> Result<serde_json::Value> {
    row.get(column_name).cloned().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "invalid column name".into(),
            details: serde_json::Value::Null,
        }),
    ))
}
// ANCHOR_END: eval_column
// ANCHOR: eval_comparison_value
fn eval_comparison_value(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    comparison_value: &models::ComparisonValue,
    root: &Row,
    item: &Row,
) -> Result<Vec<serde_json::Value>> {
    match comparison_value {
        models::ComparisonValue::Column { column } => eval_comparison_target(
            collection_relationships,
            variables,
            state,
            column,
            root,
            item,
        ),
        models::ComparisonValue::Scalar { value } => Ok(vec![value.clone()]),
        models::ComparisonValue::Variable { name } => {
            let value = variables
                .get(name.as_str())
                .ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "invalid variable name".into(),
                        details: serde_json::Value::Null,
                    }),
                ))
                .cloned()?;
            Ok(vec![value])
        }
    }
}
// ANCHOR_END: eval_comparison_value
// ANCHOR: eval_nested_field
fn eval_nested_field(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    value: serde_json::Value,
    nested_field: &models::NestedField,
) -> Result<models::RowFieldValue> {
    match nested_field {
        models::NestedField::Object(nested_object) => {
            let full_row: Row = serde_json::from_value(value).map_err(|_| {
                (
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "Expected object".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            let row = eval_row(
                &nested_object.fields,
                collection_relationships,
                variables,
                state,
                &full_row,
            )?;
            Ok(models::RowFieldValue(serde_json::to_value(row).map_err(
                |_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "Cannot encode rowset".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                },
            )?))
        }
        models::NestedField::Array(models::NestedArray { fields }) => {
            let array: Vec<serde_json::Value> = serde_json::from_value(value).map_err(|_| {
                (
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "Expected array".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            let result_array = array
                .into_iter()
                .map(|value| {
                    eval_nested_field(collection_relationships, variables, state, value, fields)
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(models::RowFieldValue(
                serde_json::to_value(result_array).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "Cannot encode rowset".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?,
            ))
        }
    }
}
// ANCHOR_END: eval_nested_field
// ANCHOR: eval_field
fn eval_field(
    collection_relationships: &BTreeMap<String, models::Relationship>,
    variables: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
    field: &models::Field,
    item: &Row,
) -> Result<models::RowFieldValue> {
    match field {
        models::Field::Column { column, fields } => {
            let col_val = eval_column(item, column.as_str())?;
            match fields {
                None => Ok(models::RowFieldValue(col_val)),
                Some(nested_field) => eval_nested_field(
                    collection_relationships,
                    variables,
                    state,
                    col_val,
                    nested_field,
                ),
            }
        }
        models::Field::Relationship {
            relationship,
            arguments,
            query,
        } => {
            let relationship = collection_relationships.get(relationship.as_str()).ok_or((
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: " ".into(),
                    details: serde_json::Value::Null,
                }),
            ))?;
            let source = vec![item.clone()];
            let collection = eval_path_element(
                collection_relationships,
                variables,
                state,
                relationship,
                arguments,
                &source,
                Some(query),
                &Some(Box::new(models::Expression::And {
                    expressions: vec![],
                })),
            )?;
            let rows = execute_query(
                collection_relationships,
                variables,
                state,
                query,
                Root::CurrentRow,
                collection,
            )?;
            let rows_json = serde_json::to_value(rows).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(models::ErrorResponse {
                        message: "cannot encode rowset".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            Ok(models::RowFieldValue(rows_json))
        }
    }
}
// ANCHOR_END: eval_field
// ANCHOR: explain
async fn post_explain(
    Json(_request): Json<models::QueryRequest>,
) -> Result<Json<models::ExplainResponse>> {
    Err((
        StatusCode::NOT_IMPLEMENTED,
        Json(models::ErrorResponse {
            message: "explain is not supported".into(),
            details: serde_json::Value::Null,
        }),
    ))
}
// ANCHOR_END: explain
// ANCHOR: mutation
async fn post_mutation(
    State(state): State<Arc<Mutex<AppState>>>,
    Json(request): Json<models::MutationRequest>,
) -> Result<Json<models::MutationResponse>> {
    let state = state.lock().await;

    let mut operation_results = vec![];

    for operation in request.operations.iter() {
        let operation_result =
            // Here, we do not want to pass a reference for the state because we don't want the tests to have the same
            // copy of the state. If we pass a reference, change in the state at one place (one mutation) will cause
            // the other tests to fail
            execute_mutation_operation(state.clone(), &request.collection_relationships, operation)
                .await?;
        operation_results.push(operation_result);
    }

    Ok(Json(models::MutationResponse { operation_results }))
}
// ANCHOR_END: mutation

async fn execute_mutation_operation(
    mut state: AppState,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    operation: &models::MutationOperation,
) -> Result<models::MutationOperationResults> {
    match operation {
        models::MutationOperation::Procedure {
            name,
            arguments,
            fields,
        } => match name.as_str() {
            "upsert_actor" => {
                let actor = arguments.get("actor").ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: " ".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let actor_obj = actor.as_object().ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: " ".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let id = actor_obj.get("id").ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: " ".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let id_int = id.as_i64().ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: " ".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let new_row =
                    BTreeMap::from_iter(actor_obj.iter().map(|(k, v)| (k.clone(), v.clone())));
                let old_row = state.actors.insert(id_int, new_row);
                Ok(models::MutationOperationResults::Procedure {
                    result: old_row.map_or(Ok(serde_json::Value::Null), |old_row| {
                        let old_row_value = serde_json::to_value(old_row).map_err(|_| {
                            (
                                StatusCode::INTERNAL_SERVER_ERROR,
                                Json(models::ErrorResponse {
                                    message: "cannot encode response".into(),
                                    details: serde_json::Value::Null,
                                }),
                            )
                        })?;

                        let old_row_fields = match fields {
                            None => Ok(models::RowFieldValue(old_row_value)),
                            Some(nested_field) => eval_nested_field(
                                collection_relationships,
                                &BTreeMap::new(),
                                &state,
                                old_row_value,
                                nested_field,
                            ),
                        }?;

                        Ok(old_row_fields.0)
                    })?,
                })
            }
            "update_actor_name_by_id" => {
                let id = arguments.get("id").ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "required argument field 'id' is missing".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let name = arguments.get("name").ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "required argument field 'name' is missing".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let id_int = id.as_i64().ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "argument 'id' is not an integer".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;

                let current_state = state.actors.clone();
                let old_row = current_state.get(&id_int);
                match &old_row {
                    Some(actor_obj) => {
                        let mut new_row = BTreeMap::from_iter(
                            actor_obj.iter().map(|(k, v)| (k.clone(), v.clone())),
                        );
                        new_row.insert("name".into(), name.clone());
                        state.actors.insert(id_int, new_row);
                        let output_row = state.actors.get(&id_int);
                        Ok(models::MutationOperationResults::Procedure {
                            result: output_row.map_or(
                                Ok(serde_json::Value::Null),
                                |output_row| {
                                    let output_row_value = serde_json::to_value(output_row)
                                        .map_err(|_| {
                                            (
                                                StatusCode::INTERNAL_SERVER_ERROR,
                                                Json(models::ErrorResponse {
                                                    message: "cannot encode response".into(),
                                                    details: serde_json::Value::Null,
                                                }),
                                            )
                                        })?;

                                    let output_row_fields = match fields {
                                        None => Ok(models::RowFieldValue(output_row_value)),
                                        Some(nested_field) => eval_nested_field(
                                            collection_relationships,
                                            &BTreeMap::new(),
                                            &state,
                                            output_row_value,
                                            nested_field,
                                        ),
                                    }?;

                                    Ok(output_row_fields.0)
                                },
                            )?,
                        })
                    }
                    None => Ok(models::MutationOperationResults::Procedure {
                        result: serde_json::Value::Null,
                    }),
                }
            }
            "uppercase_actor_name_by_id" => {
                let id = arguments.get("id").ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "required argument field 'id' is missing".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let id_int = id.as_i64().ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(models::ErrorResponse {
                        message: "argument 'id' is not an integer".into(),
                        details: serde_json::Value::Null,
                    }),
                ))?;
                let current_state = state.actors.clone();
                let old_row = current_state.get(&id_int);
                match &old_row {
                    Some(actor_obj) => {
                        let actor_name = actor_obj.get("name").ok_or((
                            StatusCode::INTERNAL_SERVER_ERROR,
                            Json(models::ErrorResponse {
                                message: "name not found".into(),
                                details: serde_json::Value::Null,
                            }),
                        ))?;
                        let actor_name_str = actor_name.as_str().ok_or((
                            StatusCode::INTERNAL_SERVER_ERROR,
                            Json(models::ErrorResponse {
                                message: "name is not a string".into(),
                                details: serde_json::Value::Null,
                            }),
                        ))?;
                        let actor_name_uppercase = actor_name_str.to_uppercase();
                        let actor_name_uppercase_value =
                            serde_json::Value::String(actor_name_uppercase);
                        let mut new_row = BTreeMap::from_iter(
                            actor_obj.iter().map(|(k, v)| (k.clone(), v.clone())),
                        );
                        new_row.insert("name".into(), actor_name_uppercase_value.clone());
                        state.actors.insert(id_int, new_row);
                        let old_row = state.actors.get(&id_int);
                        Ok(models::MutationOperationResults::Procedure {
                            result: old_row.map_or(Ok(serde_json::Value::Null), |old_row| {
                                let old_row_value =
                                    serde_json::to_value(old_row).map_err(|_| {
                                        (
                                            StatusCode::INTERNAL_SERVER_ERROR,
                                            Json(models::ErrorResponse {
                                                message: "cannot encode response".into(),
                                                details: serde_json::Value::Null,
                                            }),
                                        )
                                    })?;

                                let old_row_fields = match fields {
                                    None => Ok(models::RowFieldValue(old_row_value)),
                                    Some(nested_field) => eval_nested_field(
                                        collection_relationships,
                                        &BTreeMap::new(),
                                        &state,
                                        old_row_value,
                                        nested_field,
                                    ),
                                }?;

                                Ok(old_row_fields.0)
                            })?,
                        })
                    }
                    None => Ok(models::MutationOperationResults::Procedure {
                        result: serde_json::Value::Null,
                    }),
                }
            }
            "uppercase_all_actor_names" => {
                let mut actors_list = vec![];
                let current_state = state.actors.clone();
                for (actor_id, actor) in current_state.iter() {
                    let id_int = *actor_id;
                    let actor_name = actor.get("name").ok_or((
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "name not found".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let actor_name_str = actor_name.as_str().ok_or((
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "name is not a string".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let actor_name_uppercase = actor_name_str.to_uppercase();
                    let actor_name_uppercase_value =
                        serde_json::Value::String(actor_name_uppercase);

                    let old_row = actor;
                    let mut new_row =
                        BTreeMap::from_iter(old_row.iter().map(|(k, v)| (k.clone(), v.clone())));
                    new_row.insert("name".into(), actor_name_uppercase_value.clone());
                    state.actors.insert(id_int, new_row);
                    let output_row = state.actors.get(actor_id);
                    let returning_value =
                        output_row.map_or(Ok(serde_json::Value::Null), |returning| {
                            let returning_value =
                                serde_json::to_value(returning).map_err(|_| {
                                    (
                                        StatusCode::INTERNAL_SERVER_ERROR,
                                        Json(models::ErrorResponse {
                                            message: "cannot encode response".into(),
                                            details: serde_json::Value::Null,
                                        }),
                                    )
                                })?;
                            let returning_fields = match fields {
                                None => Ok(models::RowFieldValue(returning_value)),
                                Some(nested_field) => eval_nested_field(
                                    collection_relationships,
                                    &BTreeMap::new(),
                                    &state,
                                    returning_value,
                                    nested_field,
                                ),
                            }?;

                            Ok(returning_fields.0)
                        })?;
                    actors_list.push(returning_value);
                }
                Ok(models::MutationOperationResults::Procedure {
                    result: serde_json::Value::Array(actors_list),
                })
            }
            "uppercase_all_actor_names_return_names_list" => {
                let mut actors_list = vec![];
                let current_state = state.actors.clone();
                for (actor_id, actor) in current_state.iter() {
                    let id_int = *actor_id;
                    let actor_name = actor.get("name").ok_or((
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "name not found".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let actor_name_str = actor_name.as_str().ok_or((
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "name is not a string".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let actor_name_uppercase = actor_name_str.to_uppercase();
                    let actor_name_uppercase_value =
                        serde_json::Value::String(actor_name_uppercase);

                    let old_row = actor;
                    let mut new_row =
                        BTreeMap::from_iter(old_row.iter().map(|(k, v)| (k.clone(), v.clone())));
                    new_row.insert("name".into(), actor_name_uppercase_value.clone());
                    state.actors.insert(id_int, new_row);
                    let output_row = state.actors.get(actor_id);
                    let returning_value =
                        output_row.map_or(Ok(serde_json::Value::Null), |new_row| {
                            let name = new_row.get("name").ok_or((
                                StatusCode::INTERNAL_SERVER_ERROR,
                                Json(models::ErrorResponse {
                                    message: "name not found".into(),
                                    details: serde_json::Value::Null,
                                }),
                            ))?;
                            Ok(name.clone())
                        })?;
                    actors_list.push(returning_value);
                }
                Ok(models::MutationOperationResults::Procedure {
                    result: serde_json::Value::Array(actors_list),
                })
            }
            "noop_procedure" => Ok(models::MutationOperationResults::Procedure {
                result: serde_json::Value::String("Noop Procedure".to_string()),
            }),
            _ => Err((
                StatusCode::BAD_REQUEST,
                Json(models::ErrorResponse {
                    message: "invalid procedure name".into(),
                    details: serde_json::Value::Null,
                }),
            )),
        },
    }
}

fn eval_column_mapping(
    relationship: &models::Relationship,
    src_row: &Row,
    tgt_row: &Row,
) -> Result<bool> {
    for (src_column, tgt_column) in relationship.column_mapping.iter() {
        let src_value = eval_column(src_row, src_column)?;
        let tgt_value = eval_column(tgt_row, tgt_column)?;
        if src_value != tgt_value {
            return Ok(false);
        }
    }
    Ok(true)
}
