SELECT
    first_name,
    last_name,
    first_name || ' ' || last_name as full_name
FROM
    Authors;