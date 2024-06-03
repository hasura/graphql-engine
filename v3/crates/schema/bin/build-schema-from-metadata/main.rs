/// This is a small command-line utility that prints out the GraphQL schemas that result from an
/// engine-metadata value.
///
use std::{
    collections::BTreeSet,
    io::{stdin, Read},
    process::exit,
};

use hasura_authn_core::Role;
use lang_graphql::generate_graphql_schema::build_namespace_schemas;

pub fn main() {
    let mut metadata_string = String::new();
    let mut h = stdin();

    h.read_to_string(&mut metadata_string).unwrap();

    if metadata_string.is_empty() {
        println!("Usage: Provide a metadata json file via stdin and get the corresponding sdl schema and introspection query results on stdout.");
        exit(-1);
    }

    let metadata =
        open_dds::traits::OpenDd::deserialize(serde_json::from_str(&metadata_string).unwrap())
            .unwrap();
    let gds = schema::GDS::new_with_default_flags(metadata).unwrap();
    let sch = gds.build_schema().unwrap();

    let namespace_schemas = build_namespace_schemas(&sch).unwrap();

    let dedup_roles: BTreeSet<Role> = gds.metadata.roles.into_iter().collect();

    for role in dedup_roles {
        println!("-------------------------------:");
        println!("Now comes the SDL schema for {role}:");
        println!("-------------------------------:");
        println!();
        println!("{}", sch.generate_sdl(&role));
        println!();
        println!();
        println!();

        print!("-------------------------------:");
        print!("Now comes the query-based? schema for {role}:");
        print!("-------------------------------:");
        println!();
        print!(
            "{}",
            serde_json::ser::to_string_pretty(namespace_schemas.get(&role).unwrap()).unwrap()
        );
        println!();
        println!();
        println!();
    }

    println!("-------------------------------:");
    println!(" Debug print of schema         :");
    println!("-------------------------------:");
    println!();
    println!("{:#?}", sch);
    println!();
    println!();
    println!();
}
