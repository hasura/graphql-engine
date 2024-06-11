# jsonschema_tidying

The default schemars output can contain duplicate entries for the same type,
even when monomorphised. This package searches for (and removes) duplicate
entries, resolving any links that are broken in the process.
