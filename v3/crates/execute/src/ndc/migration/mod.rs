use thiserror::Error;

pub mod v01;

#[derive(Error, Debug)]
pub enum NdcDowngradeError {}
