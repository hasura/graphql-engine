use open_dds::{
    identifier::SubgraphName,
    relationships::{RelationshipTarget, RelationshipV1},
};

pub fn get_target_subgraph(relationship: &RelationshipV1) -> Option<SubgraphName> {
    match &relationship.target {
        RelationshipTarget::Model(model_target) => model_target.subgraph(),
        RelationshipTarget::Command(command_target) => command_target.subgraph(),
    }
}
