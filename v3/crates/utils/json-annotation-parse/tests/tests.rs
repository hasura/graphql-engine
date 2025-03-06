use jsonpath::{JSONPath, JSONPathElement};

// can be parse JSON at all?
#[test]
fn test_parse() {
    let tests = vec![
        "[]",
        "[] ",
        " []",
        "   true",
        " false",
        "[ true, true, false ]",
        "{}",
        "\"dog\"",
        "false",
        "123",
        " 123.123",
        "{ \"and\": \"and\" }",
        "{ \"and\": \"_and\" }",
        "{ \"snake_case\": \"_and\" }",
        "{ \"dog\": true }",
        "{ \"dog\": { \"dog\": true }}",
    ];

    for input in tests {
        dbg!(&input);

        let parse_result = json_annotation_parse::parse(input);

        dbg!(&parse_result);
        assert!(parse_result.is_ok());
    }
}

// given some parsed JSON, can we pull out an annotation in the middle of it?
#[test]
fn test_walk_path() {
    let tests = vec![
        (
            "[ true, false ]",
            JSONPath(vec![JSONPathElement::Index(1)]),
            "false",
        ),
        (
            "[ true, false ]",
            JSONPath(vec![JSONPathElement::Index(0)]),
            "true",
        ),
        (
            "{ \"dogs\": true }",
            JSONPath(vec![JSONPathElement::Key("dogs".into())]),
            "true",
        ),
        (
            "{ \"horse\": [ true, false ] }",
            JSONPath(vec![
                JSONPathElement::Key("horse".into()),
                JSONPathElement::Index(0),
            ]),
            "true",
        ),
        (
            "{ \"norse\": { \"horse\": [ true, false ] } }",
            JSONPath(vec![
                JSONPathElement::Key("norse".into()),
                JSONPathElement::Key("horse".into()),
                JSONPathElement::Index(0),
            ]),
            "true",
        ),
    ];

    for (input, mut path, expected) in tests {
        let parsed_json = json_annotation_parse::parse(input).unwrap();
        let found_annotation = json_annotation_parse::walk(&parsed_json, &mut path).unwrap();

        // use the annotation to pluck out the substring to see if we got the right thing
        let result = input.get(found_annotation.start.offset..found_annotation.end.offset);

        assert_eq!(result, Some(expected));
    }
}
