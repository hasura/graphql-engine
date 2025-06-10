use std::collections::BTreeMap;

use ndc_models;

use crate::{
    arguments::check_all_arguments_used,
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn collection_info() -> ndc_models::CollectionInfo {
    ndc_models::CollectionInfo {
        name: "institutions".into(),
        description: Some("A collection of institutions".into()),
        collection_type: "institution".into(),
        arguments: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "InstitutionByID".into(),
            ndc_models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
        relational_mutations: Some(ndc_models::RelationalMutationInfo {
            insertable: true,
            updatable: true,
            deletable: true,
        }),
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    check_all_arguments_used(arguments)?;
    Ok(state.institutions.values().cloned().collect())
}

pub struct Institution<'a> {
    pub name: Option<&'a str>,
    pub location: Option<Location<'a>>,
    pub staff: Option<Vec<Staff<'a>>>,
    pub departments: Option<Vec<&'a str>>,
}

pub struct Location<'a> {
    pub city: Option<&'a str>,
    pub country: Option<&'a str>,
    pub campuses: Option<Vec<&'a str>>,
}

pub struct Staff<'a> {
    pub first_name: Option<&'a str>,
    pub last_name: Option<&'a str>,
}

pub(crate) fn filter_by_institution_object_input<'a>(
    state: &'a AppState,
    institution_query: Institution<'a>,
) -> impl std::iter::Iterator<Item = Result<&'a Row>> {
    state
        .institutions
        .values()
        .map(move |institution| {
            let institution_data = parse_institution(institution)?;
            let pass = check_institution_query(&institution_query, &institution_data);
            Ok((institution, pass))
        })
        .filter_map(move |result| match result {
            Ok((institution, pass)) => {
                if pass {
                    Some(Ok(institution))
                } else {
                    None
                }
            }
            Err(err) => Some(Err(err)),
        })
}

fn check_institution_query<'a>(
    institution_query: &'a Institution<'a>,
    institution_data: &'a Institution<'a>,
) -> bool {
    // check name
    if let Some(query_name) = institution_query.name {
        if let Some(data_name) = institution_data.name {
            if data_name == query_name {
                return true;
            }
        }
    }

    // check location
    if let Some(query_location) = &institution_query.location {
        if let Some(data_location) = &institution_data.location {
            // location -> city
            if let Some(query_city) = query_location.city {
                if let Some(data_city) = data_location.city {
                    if data_city == query_city {
                        return true;
                    }
                }
            }
            // location -> country
            if let Some(query_country) = query_location.country {
                if let Some(data_country) = data_location.country {
                    if data_country == query_country {
                        return true;
                    }
                }
            }
            // location -> campuses
            if let Some(query_campuses) = &query_location.campuses {
                if let Some(data_campuses) = &data_location.campuses {
                    for query_campus in query_campuses {
                        for data_campus in data_campuses {
                            if query_campus == data_campus {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }

    // TODO: implement filters for the other fields

    false
}

pub fn parse_institution(institution: &Row) -> Result<Institution<'_>> {
    let name = institution.get("name").and_then(|name| name.as_str());

    let staff = institution
        .get("staff")
        .and_then(|staff| parse_staff_members(staff));

    let location = institution.get("location").and_then(parse_location);

    let departments = institution.get("departments").and_then(parse_departments);

    Ok(Institution {
        name,
        location,
        staff,
        departments,
    })
}

pub fn parse_institution_query(
    institution: &serde_json::Map<String, serde_json::Value>,
) -> Result<Institution<'_>> {
    let name = institution.get("name").and_then(|name| name.as_str());

    let staff = institution
        .get("staff")
        .and_then(|staff| parse_staff_members(staff));

    let location = institution.get("location").and_then(parse_location);

    let departments = institution.get("departments").and_then(parse_departments);

    Ok(Institution {
        name,
        location,
        staff,
        departments,
    })
}

fn parse_location(location: &serde_json::Value) -> Option<Location> {
    if *location == serde_json::Value::Null {
        return None;
    }
    let city = location.get("city").and_then(|n| n.as_str());
    let country = location.get("country").and_then(|n| n.as_str());
    let campuses = location.get("campuses").and_then(|n| n.as_array());
    if city.is_none() && country.is_none() && campuses.is_none() {
        None
    } else {
        let campuses = campuses.map(|campuses| {
            campuses
                .iter()
                .filter_map(|val| val.as_str())
                .collect::<Vec<_>>()
        });
        Some(Location {
            city,
            country,
            campuses,
        })
    }
}

fn parse_staff_members(value: &serde_json::Value) -> Option<Vec<Staff>> {
    if *value == serde_json::Value::Null {
        None
    } else {
        value.as_array().map(|value| {
            value
                .iter()
                .filter_map(parse_staff_member)
                .collect::<Vec<_>>()
        })
    }
}

fn parse_departments(value: &serde_json::Value) -> Option<Vec<&str>> {
    if *value == serde_json::Value::Null {
        None
    } else {
        value.as_array().map(|value| {
            value
                .iter()
                .filter_map(|value| value.as_str())
                .collect::<Vec<_>>()
        })
    }
}

fn parse_staff_member(staff: &serde_json::Value) -> Option<Staff> {
    if *staff == serde_json::Value::Null {
        return None;
    }
    let first_name = staff.get("first_name").and_then(|n| n.as_str());
    let last_name = staff.get("last_name").and_then(|n| n.as_str());
    if first_name.is_none() && last_name.is_none() {
        None
    } else {
        Some(Staff {
            first_name,
            last_name,
        })
    }
}
