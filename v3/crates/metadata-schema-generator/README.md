# Metadata Schema Generator

When we make changes to the metadata schema, we need to regenerate the
goldenfile to make the tests happy. We can use the following command to get the
schema immediately for whatever reason:

```bash
$ cargo run --bin metadata-schema-generator
```
