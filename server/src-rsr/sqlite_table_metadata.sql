SELECT
  m."name"    as table_name,
  p."name"    as column_name,
  p."cid"     as column_index,
  p."type"    as column_type,
  p."notnull" as column_notnull
FROM
  sqlite_master AS m
JOIN
  pragma_table_info(m.name) AS p
WHERE
  m.type = 'table'
ORDER BY
  m.name,
  p.cid
