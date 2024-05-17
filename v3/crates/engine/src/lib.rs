pub mod authentication;
pub mod build;

// This is set by the build.rs script.
/// The version of the v3-engine release.
pub static VERSION: &str = env!(
    "CARGO_V3_ENGINE_VERSION",
    "Unable to start engine: unable to fetch the current git hash to use as version in traces"
);
