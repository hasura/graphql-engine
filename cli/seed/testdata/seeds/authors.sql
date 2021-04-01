CREATE TABLE authors
(
    id   SERIAL PRIMARY KEY,
    name TEXT
);

INSERT INTO authors(id, name)
VALUES (1, 'test1'),
        (4, 'test2');

