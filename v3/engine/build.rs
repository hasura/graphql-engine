fn main() {
    // Cargo sets the PROFILE environment variable
    let profile = std::env::var("PROFILE").unwrap();
    if profile == "release" {
        // For release builds (cargo build --release ...), set the version to the git commit short
        let commit_short = build_data::get_git_commit_short().unwrap();
        println!("cargo:rustc-env=CARGO_V3_ENGINE_VERSION={}", commit_short);
    } else {
        // For non-release builds, set the version to 'dev'
        println!("cargo:rustc-env=CARGO_V3_ENGINE_VERSION=dev");
    }
}
