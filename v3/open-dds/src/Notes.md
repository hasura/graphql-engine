Notes.md 

Should supergraph object be part of MetadataAccessor?
  -> The accessor has all the metadata objects across the subgraphs, flags.
  -> Yes, this should contain the supergraph object. Supergraph objects will not be a vector

What should the empty accessor look like for supergraph objects?
    -> While resolving metadata accessor I want to check if graphql cofig is present or not
    -> And then use the logic to see if default graphql config should be applied
    -> I want to check if graphql config is present OR not.
    -> This should be a vector

1. Revert the changes from ModelRelationshipAnnotation
2. LocalModelRelationships will now take an enum of relatoinshipAnnotation which can either be anything