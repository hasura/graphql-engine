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

ALTER TABLE ONLY public.movie_analytics ALTER COLUMN id SET DEFAULT nextval('public.movie_analytics_id_seq'::regclass);
