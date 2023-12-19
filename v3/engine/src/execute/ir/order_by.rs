use lang_graphql::normalized_ast::{self as normalized_ast, InputField};
use ndc_client as gdc;

use crate::schema::types::{Annotation, InputAnnotation, ModelInputAnnotation};

use crate::execute::error;
use crate::schema::types;
use crate::schema::GDS;

pub fn build_ndc_order_by(
    args_field: &InputField<GDS>,
) -> Result<gdc::models::OrderBy, error::Error> {
    match &args_field.value {
        normalized_ast::Value::Object(arguments) => {
            let mut ndc_order_elements = Vec::new();
            for (_name, argument) in arguments {
                match argument.info.generic {
                    Annotation::Input(InputAnnotation::Model(
                        types::ModelInputAnnotation::ModelOrderByArgument { ndc_column },
                    )) => {
                        let order_by_value = argument.value.as_enum()?;
                        let order_direction = match &order_by_value.info.generic {
                            Annotation::Input(InputAnnotation::Model(
                                ModelInputAnnotation::ModelOrderByDirection { direction },
                            )) => match &direction {
                                types::ModelOrderByDirection::Asc => {
                                    gdc::models::OrderDirection::Asc
                                }
                                types::ModelOrderByDirection::Desc => {
                                    gdc::models::OrderDirection::Desc
                                }
                            },
                            &annotation => {
                                return Err(error::InternalEngineError::UnexpectedAnnotation {
                                    annotation: annotation.clone(),
                                })?
                            }
                        };

                        let order_element = gdc::models::OrderByElement {
                            order_direction,
                            target: gdc::models::OrderByTarget::Column {
                                name: ndc_column.clone(),
                                path: Vec::new(),
                            },
                        };

                        ndc_order_elements.push(order_element);
                    }
                    annotation => {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        })?;
                    }
                }
            }
            Ok(gdc::models::OrderBy {
                elements: ndc_order_elements,
            })
        }
        _ => Err(error::InternalEngineError::InternalGeneric {
            description: "Expected object value for model arguments".into(),
        })?,
    }
}
