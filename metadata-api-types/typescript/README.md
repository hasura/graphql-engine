# Hasura GraphQL Engine Metadata API Types

This package contains TypeScript types for working with the Hasura GraphQL
Engine Metadata API. That is the API that is used to configure the GraphQL
Engine.

Currently this library exports types used for exporting and importing GraphQL
Engine metadata in its entirety. To get the root type for exported metadata,
import the `MetadataV3` type.

```ts
import type { MetadataV3 } from '@hasura/metadata-api'
```

The other types provided by the library are aliases for types of properties that
appear within the metadata export.
