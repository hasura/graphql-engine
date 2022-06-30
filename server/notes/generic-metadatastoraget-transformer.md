This note is in [Hasura.Metadata.Class](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Metadata/Class.hs#L335).
It is referenced at:
  - line 1147 of [Hasura.App](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/App.hs#L1147)
  - line 381 of [Hasura.Metadata.Class](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Metadata/Class.hs#L381)

# Generic MetadataStorageT transformer

All methods of the MonadMetadataStorage class may fail, which we represent in
the usual way using a MonadError superclass:

    class MonadError QErr m => MonadMetadataStorage m

However, unusually, the location where we pick a concrete MonadMetadataStorage
instance is not a context where we can handle errors, and as such the monad at
that point has no MonadError instance! Instead, clients of MonadMetadataStorage
are expected to handle errors /locally/, even though the code is parameterized
over an arbitrary metadata storage mechanism.

To encode this, we take a slightly unorthodox approach involving the auxiliary
MetadataStorageT transformer, which is really just a wrapper around ExceptT:

    newtype MetadataStorageT m a
      = MetadataStorageT { unMetadataStorageT :: ExceptT QErr m a }

We then define MonadMetadataStorage instances on a transformer stack comprising
both MetadataStorageT and a concrete base monad:

    instance MonadMetadataStorage (MetadataStorageT PGMetadataStorageApp)

This looks unconventional, but it allows polymorphic code to be parameterized
over the metadata storage implementation while still handling errors locally.
Such functions include a constraint of the form

    MonadMetadataStorage (MetadataStorageT m) => ...

and use runMetadataStorageT at the location where errors should be handled, e.g.:

    result <- runMetadataStorageT do
      {- ... some metadata operations ...
