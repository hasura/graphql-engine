
DROP PROCEDURE IF EXISTS search_author_mviewf;
DROP TABLE IF EXISTS article;
DROP TABLE IF EXISTS author;

CREATE TABLE author
(
    id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(45) UNIQUE KEY,
    createdAt DATETIME
);

INSERT INTO author
    (name, createdAt)
VALUES
    ( 'Author 1', '2017-09-21 09:39:44' ),
    ( 'Author 2', '2017-09-21 09:50:44' );

CREATE TABLE article (
    id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    title TEXT,
    content TEXT,
    is_published BIT,
    published_on TIMESTAMP,
    author_id INT UNSIGNED,
    co_author_id INT UNSIGNED,
    FOREIGN KEY (author_id) REFERENCES author(id),
    FOREIGN KEY (co_author_id) REFERENCES author(id)
);

INSERT INTO article
    (title, content, author_id, is_published)
VALUES
    ( 'Article 1', 'Sample article content 1', 1, 0 ),
    ( 'Article 2', 'Sample article content 2', 1, 1 ),
    ( 'Article 3', 'Sample article content 3', 2, 1 );

CREATE OR REPLACE VIEW search_author_view AS
  SELECT * FROM author;

CREATE PROCEDURE search_author_viewf (query TEXT)
   SELECT * FROM search_author_view WHERE name = query;
