/*****************************************
Non chinook tables
******************************************/

CREATE TABLE public.article (
    id integer NOT NULL,
    title text,
    author_id integer,
    CONSTRAINT article_pkey PRIMARY KEY (id)
);

COPY public.article (id, title, author_id) FROM stdin;
1	The Next 700 Programming Languages	1
2	Why Functional Programming Matters	2
3	The Design And Implementation Of Programming Languages	2
4	The Mechanical Evaluation of Expressions	1
5	Generalizing monads to arrows	2
\.

CREATE OR REPLACE FUNCTION public.search_articles(search_term text)
RETURNS SETOF public.article
LANGUAGE plpgsql
STABLE
AS $$
BEGIN
    RETURN QUERY
    SELECT * FROM public.article
    WHERE title ILIKE '%' || search_term || '%';
END;
$$;

CREATE TABLE author (
  id integer NOT NULL,
  first_name text,
  last_name text,
  CONSTRAINT author_pkey PRIMARY KEY (id)
);

COPY public.author (id, first_name, last_name) FROM stdin;
1	Peter	Landin
2	John	Hughes
\.


CREATE TABLE public.movie_analytics (
    id integer NOT NULL,
    movie_id integer NOT NULL,
    num_votes_day integer,
    num_views_day integer,
    num_users_faved integer,
    num_users_watchlisted integer,
    total_votes integer,
    prev_day_scores integer,
    movie_name text
);


CREATE SEQUENCE public.movie_analytics_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.movie_analytics_id_seq OWNER TO postgres;

--
-- Data for Name: movie_analytics; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.movie_analytics (id, movie_id, num_votes_day, num_views_day, num_users_faved, num_users_watchlisted, total_votes, prev_day_scores, movie_name) FROM stdin;
1	1	10	22	9	11	578	8	Titanic
2	2	8	13	4	3	432	7	Slumdog Millionaire
3	3	17	34	14	11	849	9	Godfather
\.

--
-- Name: movie_analytics movie_analytics_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.movie_analytics
ADD CONSTRAINT movie_analytics_pkey PRIMARY KEY (id);


--
-- Name: article FK_article_author_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--


--
-- Name: movie_analytics_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.movie_analytics_id_seq', 3, true);


--
-- Name: article article_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--


--
-- Name: movie_analytics_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.movie_analytics_id_seq OWNED BY public.movie_analytics.id;

--
-- Name: movie_analytics id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.movie_analytics
ALTER COLUMN id
SET DEFAULT nextval('public.movie_analytics_id_seq'::regclass);
-- `institution` table for testing queries into JSON objects
CREATE TYPE public.city AS (name text);
CREATE TYPE public.country AS (name text, continent text, cities city []);
CREATE TYPE public.location AS (city text, country country, campuses text []);
CREATE TYPE public.pet AS (name text, age int);
CREATE TYPE public.staff AS (
  first_name text,
  last_name text,
  specialities text [],
  pets pet [],
  favourite_artist_id int
);
CREATE TYPE public.institution_songs AS (
  primary_anthem_track_id int,
  secondary_anthem_track_id int
);
CREATE TABLE public.institution (
  id integer NOT NULL,
  name text,
  location location,
  staff staff [],
  departments text [],
  songs institution_songs,
  CONSTRAINT institution_pkey PRIMARY KEY (id)
);
INSERT INTO public.institution (id, name, location, staff, departments, songs)
VALUES (
    1,
    'Queen Mary University of London',
    ROW(
      'London',
      ROW(
        'UK',
        'Europe',
        ARRAY [ROW('London')::city, ROW('Birmingham')::city]
      )::country,
      ARRAY ['Mile End','Whitechapel','Charterhouse Square','West Smithfield']
    )::location,
    ARRAY [ROW('Peter','Landin',
      ARRAY['Computer Science','Education'],
    ARRAY [('Dog',100) ::pet],
    1
  )::staff ],
  ARRAY ['Humanities and Social Sciences','Science and Engineering','Medicine and Dentistry'],
  ROW(2270, 2271)::institution_songs
),
(
  2,
  'Chalmers University of Technology',
  ROW(
    'Gothenburg',
    ROW(
      'Sweden',
      'Europe',
      ARRAY [ROW('Gothenburg')::city, ROW('Stockholm')::city]
    )::country,
    ARRAY ['Johanneberg','Lindholmen']
  )::location,
  ARRAY [ROW('John','Hughes',
      ARRAY['Computer Science', 'Functional Programming', 'Software Testing'],
  ARRAY [('Horse', 20) :: pet],
  2
)::staff,
ROW(
  'Koen',
  'Claessen',
  ARRAY ['Computer Science', 'Functional Programming', 'Automated Reasoning'],
  ARRAY [('First horse',1) :: pet, ('Second horse', 2) ::pet],
  3
)::staff ],
ARRAY ['Architecture and Civil Engineering','Computer Science and Engineering','Electrical Engineering','Physics','Industrial and Materials Science'],
ROW(3421, NULL)::institution_songs
),
(
  3,
  'University of Nowhere',
  null,
  null,
  ARRAY ['nothing',null],
  NULL
);
