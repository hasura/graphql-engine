This note is in [Hasura.RQL.DDL.Warnings](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Warnings.hs#L36).

# Warnings in metadata API

The metadata API handlers return EncJSON, which is just a Bytestring Builder. Now, in order to add warnings to the API
response, we cannot use the `runMetadataWarnings` at the top level (i.e. in `runMetadataQueryM`) as appending something
to the EncJSON will require us to parse the JSON and then re-serialize it. This is wasteful and we should avoid it.

As a result, we are using the `MonadWarnings` class to add warnings at the API handler level, i.e., the API handler will
use the runMetadataWarnings function to run the handler and get the warnings. Then, the API handler will use the warnings
to construct the response.

We can however avoid this by changing the return type of the metadata API handlers to something like:

> data MetadataAPIOutput =
>     RawOutput EncJSON
>   | SuccessWithWarnings MetadataWarnings
>   | InconsistentMetadataWithWarnings MetadataWarnings

This will allow us to cater to the metadata APIs:
- That contacts some external service and passes the raw response (like the export_metadata API).
- That returns a success message with warnings (like the replace_metadata v1 API).
- That returns inconsistent metadata with warnings (like the replace_metadata v2 API).

Also, we can expand the scope of `MetadataAPIOutput` to include other types of responses as well in the future.

