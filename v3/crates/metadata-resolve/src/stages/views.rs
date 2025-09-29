use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use open_dds::views::ViewV1;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ResolvedView {
    pub view: ViewV1,
}

#[derive(Debug)]
pub struct ViewsOutput {
    pub views: IndexMap<Qualified<open_dds::views::ViewName>, ResolvedView>,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error("Circular dependency detected for view {view_name}")]
    DuplicateViewDefinition {
        view_name: Qualified<open_dds::views::ViewName>,
    },
}

pub fn resolve(
    views: &Vec<open_dds::accessor::QualifiedObject<open_dds::views::ViewV1>>,
) -> Result<ViewsOutput, Error> {
    let mut resolved_views = IndexMap::new();

    // Collect all views across all subgraphs
    for view_object in views {
        let qualified_name: Qualified<open_dds::views::ViewName> = Qualified::new(
            view_object.subgraph.clone(),
            view_object.object.name.clone(),
        );
        if let Some(_existing) = resolved_views.insert(
            qualified_name.clone(),
            ResolvedView {
                view: view_object.object.clone(),
            },
        ) {
            return Err(Error::DuplicateViewDefinition {
                view_name: qualified_name,
            });
        }
    }

    Ok(ViewsOutput {
        views: resolved_views,
    })
}
