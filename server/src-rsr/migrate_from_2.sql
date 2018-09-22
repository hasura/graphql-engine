CREATE TABLE hdb_catalog.hdb_function
(
    function_schema TEXT,
    function_name TEXT,
    is_system_defined boolean default false,

    PRIMARY KEY (function_schema, function_name)
);
