// This is copy/pasted into MBS so we can avoid pulling in the entire engine crate. If you want to
// update this script, please update the other one too.

fn main() -> Result<(), String> {
    // Ensure that we rebuild if the version is specified.
    println!("cargo:rerun-if-env-changed=RELEASE_VERSION");

    // On release builds, use the Git commit as a backup if the version is not set.
    // On debug builds, use "dev".
    // If we fail to get the Git information, fail.
    let build_profile = std::env::var("PROFILE").map_err(|err| err.to_string())?;
    let release_version = if build_profile == "release" {
        if let Some(version) = option_env!("RELEASE_VERSION") {
            version.to_string()
        } else {
            let git_commit_ref = build_data::get_git_commit_short()?;
            let git_dirty = build_data::get_git_dirty().unwrap_or(false);
            if git_dirty {
                format!("{git_commit_ref}-dirty")
            } else {
                git_commit_ref
            }
        }
    } else {
        "dev".to_string()
    };
    println!("cargo:rustc-env=CARGO_V3_ENGINE_VERSION={release_version}");
    Ok(())
}
