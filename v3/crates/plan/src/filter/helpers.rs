use open_dds::data_connector::DataConnectorColumnName;

#[test]
fn test_with_nesting_path() {
    let path = [
        &DataConnectorColumnName::new("location".into()),
        &DataConnectorColumnName::new("country".into()),
    ];
    let column = DataConnectorColumnName::new("cities".into());

    assert_eq!(
        with_nesting_path(&column, &path),
        (
            "location".into(),
            ["country".into(), "cities".into()].to_vec()
        )
    );
}

#[test]
fn test_with_nesting_path_empty_path() {
    let path = [];
    let column = DataConnectorColumnName::new("cities".into());

    assert_eq!(
        with_nesting_path(&column, &path),
        ("cities".into(), [].to_vec())
    );
}

// if the nesting path is empty, return the head
// if there are items in the nesting path, return the first item
// and put the head item at the start of nesting path
// useful when constructing field paths
pub fn with_nesting_path(
    head: &DataConnectorColumnName,
    path: &[&DataConnectorColumnName],
) -> (DataConnectorColumnName, Vec<DataConnectorColumnName>) {
    // The column name is the root column
    let new_head = path.first().map_or(head, |v| v);

    let new_path = path
        .iter()
        .copied()
        .chain([head])
        .skip(1)
        .cloned()
        .collect();
    (new_head.clone(), new_path)
}
