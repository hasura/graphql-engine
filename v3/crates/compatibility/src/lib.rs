mod compatibility_date;
pub use compatibility_date::{
    get_compatibility_date_for_flag, new_compatibility_date, CompatibilityDate,
    OLDEST_COMPATIBILITY_DATE,
};

mod config;
pub use config::{CompatibilityConfigV1, CompatibilityConfigV2, CompatibilityError};
