use crate::{Error, WithContext};

// for each error (single or multiple)
// try and turn it into a fancy ariadne Report
// or fallback to a plain one
pub fn to_fancy_errors<'a>(
    raw_metadata: &'a str,
    error: &WithContext<Error>,
    config: ariadne::Config,
) -> Vec<ariadne::Report<'a>> {
    match error.into_inner() {
        Error::MultipleErrors { errors } => errors
            .lines_of
            .iter()
            .map(|e| {
                to_fancy_error(raw_metadata, e, config)
                    .unwrap_or_else(|| to_fallback_error(e.into_inner(), config))
            })
            .collect(),

        _ => {
            vec![
                to_fancy_error(raw_metadata, error, config)
                    .unwrap_or_else(|| to_fallback_error(error.into_inner(), config)),
            ]
        }
    }
}

// given a metadata resolve error, try and turn it into a fancy ariadne Report
// this may fail if the error has no context, or we can't parse the JSON
fn to_fancy_error<'a>(
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
                for item in &context.0 {
                    let location =
                        json_annotation_parse::walk(&fancy_json, &mut item.path.clone()).ok()?;
                    first_location = location.start.offset..location.end.offset;

                    labels.push(
                        ariadne::Label::new(location.start.offset..location.end.offset)
                            .with_message(item.message.clone())
                            .with_color(ariadne::Color::Red),
                    );
                }
                Some(
                    ariadne::Report::build(ariadne::ReportKind::Error, first_location)
                        .with_message(error.to_string())
                        .with_config(config)
                        .with_labels(labels)
                        .finish(),
                )
            })
    } else {
        None
    }
}

fn to_fallback_error(error: &Error, config: ariadne::Config) -> ariadne::Report<'static> {
    ariadne::Report::build(ariadne::ReportKind::Error, 0..0)
        .with_message(error.to_string())
        .with_config(config)
        .finish()
}
