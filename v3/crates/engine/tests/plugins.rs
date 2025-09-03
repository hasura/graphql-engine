use std::collections::BTreeMap;

use metadata_resolve::data_connectors::NdcVersion;

mod common;

// pre-ndc-request-plugin

#[test]
fn test_plugin_pre_parse_plugin_pass_through() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/plugins/pre_parse/pass_through",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_plugin_pre_ndc_request_plugin_pass_through() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/plugins/pre_ndc_request/pass_through",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_plugin_pre_ndc_request_plugin_send_session_variable() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/plugins/pre_ndc_request/send_session_variable",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_plugin_pre_ndc_request_plugin_send_header() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/plugins/pre_ndc_request/send_header",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// pre-ndc-response-plugin

#[test]
fn test_plugin_pre_ndc_response_plugin_pass_through() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/plugins/pre_ndc_response/pass_through",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}
