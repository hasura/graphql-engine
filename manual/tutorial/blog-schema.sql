CREATE TABLE author (
    -- hasura_id is used to link to the user generated on the auth service
    hasura_id INTEGER NOT NULL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE article (
    id SERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    content TEXT NOT NULL,
    is_published BOOLEAN NOT NULL DEFAULT false,
    author_id INTEGER NOT NULL REFERENCES author(hasura_id)
);

CREATE TABLE article_like (
    article_id INTEGER NOT NULL REFERENCES article(id),
    author_id INTEGER NOT NULL REFERENCES author(hasura_id),
    PRIMARY KEY (author_id, article_id)
);

CREATE TABLE comment (
    id SERIAL NOT NULL PRIMARY KEY,
    author_id INTEGER NOT NULL REFERENCES author(hasura_id),
    article_id INTEGER NOT NULL REFERENCES article(id),
    comment TEXT NOT NULL
);

CREATE TABLE category (
    id SERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE article_category (
    article_id INTEGER NOT NULL REFERENCES article(id),
    category_id INTEGER NOT NULL REFERENCES category(id),
    PRIMARY KEY (article_id, category_id)
);
