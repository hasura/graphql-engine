CREATE TABLE articles
(
    id        serial  NOT NULL,
    title     text    NOT NULL,
    content   text    NOT NULL,
    rating    integer NOT NULL,
    author_id serial  NOT NULL,
    PRIMARY KEY (id)
);

INSERT INTO articles (id, title, content, rating, author_id)
VALUES (1, 'test1', 'test1', 1, 4),
        (2, 'test2', 'test1', 1, 4),
        (3, 'test3', 'test1', 1, 4);