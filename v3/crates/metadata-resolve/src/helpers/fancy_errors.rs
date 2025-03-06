use crate::{Error, WithContext};

// given a metadata resolve error, try and turn it into a fancy ariadne Report
// this may fail if the error has no context, or we can't parse the JSON
pub fn to_fancy_error<'a>(
    raw_metadata: &'a str,
    error: &WithContext<Error>,
    config: ariadne::Config,
) -> Option<ariadne::Report<'a>> {
    if let WithContext::Contextualised { error, context } = error {
        json_annotation_parse::parse(raw_metadata)
            .ok()
            .and_then(|fancy_json| {
                let mut labels = vec![];

                let mut first_location = 0..0;
                for item in context.0.clone() {
                    let location =
                        json_annotation_parse::walk(&fancy_json, &mut item.path.clone()).ok()?;
                    first_location = location.start.offset..location.end.offset;

                    labels.push(
                        ariadne::Label::new(location.start.offset..location.end.offset)
                            .with_message(item.message)
                            .with_color(ariadne::Color::Red),
                    );
                }
                Some(
                    ariadne::Report::build(ariadne::ReportKind::Error, first_location)
                        .with_config(config)
                        .with_message(error.to_string())
                        .with_labels(labels)
                        .finish(),
                )
            })
    } else {
        None
    }
}
