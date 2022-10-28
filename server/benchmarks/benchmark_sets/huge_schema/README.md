**Huge Schema** is an anonymized version of a real schema, without row data. It
is notable for:

- having hundreds of tables, and relationships (including cycles)
- dozens of different roles and permissions

We select it for inclusion because the memory requirements to serve such a
large schema are currently high.

We also test a small and a big query (returning no data) to see if schema size
affects latency and (for the latter case) to time SQL generation.
