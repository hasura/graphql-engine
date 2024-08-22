use super::compatibility_date::CompatibilityDate;
use super::types::CompatibilityConfig;
use chrono::NaiveDate;
use metadata_resolve::configuration::WarningsToRaise;

pub const fn new_compatibility_date(year: i32, month: u32, day: u32) -> CompatibilityDate {
    CompatibilityDate(match NaiveDate::from_ymd_opt(year, month, day) {
        // Need to match instead of unwrap because unwrap is still unstable as const
        Some(date) => date,
        None => panic!("Invalid date"),
    })
}

/// Resolve `CompatibilityConfig` which is not part of metadata. Hence we resolve/build
/// it separately.
pub fn resolve_compatibility_config(
    raw_compatibility_config: &str,
) -> Result<CompatibilityConfig, anyhow::Error> {
    Ok(open_dds::traits::OpenDd::deserialize(
        serde_json::from_str(raw_compatibility_config)?,
    )?)
}

// given compatibility config, work out which warnings becomes errors using the date
pub fn config_to_metadata_resolve(compat_config: &Option<CompatibilityConfig>) -> WarningsToRaise {
    match compat_config {
        Some(CompatibilityConfig::V1(compat_config_v1)) => {
            warnings_to_raise_from_date(&compat_config_v1.date)
        }
        Some(CompatibilityConfig::V2(compat_config_v2)) => {
            warnings_to_raise_from_date(&compat_config_v2.date)
        }

        None => WarningsToRaise::default(),
    }
}

// note we have no warnings to raise yet, so this is a no-op whilst we get the plumbing sorted
fn warnings_to_raise_from_date(_date: &CompatibilityDate) -> WarningsToRaise {
    WarningsToRaise {
            // some_boolean_option: date >= &new_compatibility_date(2024, 1, 1) 

        }
}
