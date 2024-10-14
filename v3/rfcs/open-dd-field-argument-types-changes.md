# Open DD field arguments

## Motivation

With the introduction of field arguments in
[NDC spec v0.1.3](https://github.com/hasura/ndc-spec/releases/tag/v0.1.3),
v3-engine is now unblocked to support field level arguments. For example, the
following GraphQL queries can be supported now:

```graphql
query {
  Artist(limit: 10) {
    Name(full: true) # return full name
    ArtistId
    CryptoHash(algorithm: SHA1)
  }
}
```

For this we need to support field arguments for OpenDD `ObjectType` kind.

Apart from the field argument definition (used for preparing the graphql
schema), we also need to define the field argument mapping as well (similar to
how we define the `fieldMapping`). The mappings will be used while preparing the
ndc query for a given query with field arguments.

## Proposal

There are a lot of ways in which this can be implemented:

1. Add a new OpenDD kind called `FieldArgument`:

   ```yaml
   kind: FieldArgument
   version: v1
   definition:
     name: Artist_Name_Field_Argument
     arguments:
       - name: full
         argumentType: Bool
       - name: initialsOnly
         argumentType: Bool
       - name: truncateAfter
         argumentType: Int
     dataConnectorArgumentsMapping:
       - dataConnectorName: pg_1
         dataConnectorObjectType: artist
         dataConnectorFieldName: name
         argumentMapping:
           full: full_name
           initialsOnly: convert_to_initials
           truncateAfter: truncate_length
   ```

   This approach is going to result in a lot of new OpenDD metadata objects
   being added to an already humongous hml file. This will be tedious to
   maintain (imagine removing an `ObjectType` and missing one of the many
   `FieldArgument`).

   Moreover the field arguments seems like a thing that should be defined while
   defining the OpenDD `ObjectType` for the NDC object type. Thus, we have the
   next proposal.

2. Extend the `ObjectType`:
   ```yaml
   kind: ObjectType
   version: v1
   definition:
     name: Artist
     graphql:
       typeName: artist
       inputTypeName: artistInput
       apolloFederation:
         keys:
           - fields:
               - ArtistId
     fields:
       - name: ArtistId
         type: int4!
       - name: Name
         type: varchar
         arguments:
           - name: full
             argumentType: Bool
           - name: initialsOnly
             argumentType: Bool
           - name: truncateAfter
             argumentType: Int
       - name: CryptoHash
         type: varchar
         arguments:
           - name: algorithm
             argumentType: varchar!
     dataConnectorTypeMapping:
       - dataConnectorName: db
         dataConnectorObjectType: Artist
         fieldMapping:
           ArtistId:
             column:
               name: ArtistId
           Name:
             column:
               name: Name
             argumentMapping:
               full: full_name
               initialsOnly: convert_to_initials
               truncateAfter: truncate_length
           CryptoHash:
             column:
               name: hash
             argumentMapping:
               - algorithm: algo
   ```

### What all will change?

Assuming we are going with the 2nd proposal (extending the `ObjectType`)

1. We will add a new field `arguments` (`in definition.fields[i]`) containing
   the definition for the arguments for the field. The definition for an
   argument consists of the following:
   1. Name of the argument (this will be used as the graphql name for the input
      field)
   2. Argument type: The graphql type for the argument.
   3. (Optional) Default value
2. We will also add a new field `argumentMapping` (in
   `definition.dataConnectorTypeMapping[i].fieldMapping.<field>`). The argument
   mapping will define the mapping between the arguments defined in the
   definition (1) to the data connector argument names.
