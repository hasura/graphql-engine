--
-- PostgreSQL database dump
--

-- Dumped from database version 11.2 (Ubuntu 11.2-1.pgdg16.04+1)
-- Dumped by pg_dump version 11.2
--
-- Name: public; Type: SCHEMA; Schema: -; Owner: rxrngbplmkiody
--

CREATE SCHEMA public;


ALTER SCHEMA public OWNER TO rxrngbplmkiody;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: rxrngbplmkiody
--

COMMENT ON SCHEMA public IS 'standard public schema';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: articles; Type: TABLE; Schema: public; Owner: rxrngbplmkiody
--

CREATE TABLE public.articles (
    id integer NOT NULL,
    title text NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    length integer NOT NULL
);


ALTER TABLE public.articles OWNER TO rxrngbplmkiody;

--
-- Name: article_id_seq; Type: SEQUENCE; Schema: public; Owner: rxrngbplmkiody
--

CREATE SEQUENCE public.article_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.article_id_seq OWNER TO rxrngbplmkiody;

--
-- Name: article_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: rxrngbplmkiody
--

ALTER SEQUENCE public.article_id_seq OWNED BY public.articles.id;


--
-- Name: comments; Type: TABLE; Schema: public; Owner: rxrngbplmkiody
--

CREATE TABLE public.comments (
    uuid uuid DEFAULT public.gen_random_uuid() NOT NULL,
    article_id integer NOT NULL,
    comment text NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.comments OWNER TO rxrngbplmkiody;

--
-- Name: likes; Type: TABLE; Schema: public; Owner: rxrngbplmkiody
--

CREATE TABLE public.likes (
    uuid uuid DEFAULT public.gen_random_uuid() NOT NULL,
    article_id integer NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.likes OWNER TO rxrngbplmkiody;

--
-- Name: article_stats; Type: VIEW; Schema: public; Owner: rxrngbplmkiody
--

CREATE VIEW public.article_stats AS
 SELECT a.id,
    a.title,
    a.length,
    l.count AS num_likes,
    c.count AS num_comments
   FROM public.articles a,
    ( SELECT likes.article_id,
            count(likes.uuid) AS count
           FROM public.likes
          GROUP BY likes.article_id) l,
    ( SELECT comments.article_id,
            count(comments.uuid) AS count
           FROM public.comments
          GROUP BY comments.article_id) c
  WHERE ((a.id = l.article_id) AND (a.id = c.article_id));


ALTER TABLE public.article_stats OWNER TO rxrngbplmkiody;

--
-- Name: stocks; Type: TABLE; Schema: public; Owner: rxrngbplmkiody
--

CREATE TABLE public.stocks (
    ticker text NOT NULL,
    price numeric NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.stocks OWNER TO rxrngbplmkiody;

--
-- Name: articles id; Type: DEFAULT; Schema: public; Owner: rxrngbplmkiody
--

ALTER TABLE ONLY public.articles ALTER COLUMN id SET DEFAULT nextval('public.article_id_seq'::regclass);


--
-- Name: articles article_pkey; Type: CONSTRAINT; Schema: public; Owner: rxrngbplmkiody
--

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT article_pkey PRIMARY KEY (id);


--
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: rxrngbplmkiody
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (uuid);


--
-- Name: likes likes_pkey; Type: CONSTRAINT; Schema: public; Owner: rxrngbplmkiody
--

ALTER TABLE ONLY public.likes
    ADD CONSTRAINT likes_pkey PRIMARY KEY (uuid);


--
-- Name: stocks stocks_pkey; Type: CONSTRAINT; Schema: public; Owner: rxrngbplmkiody
--

ALTER TABLE ONLY public.stocks
    ADD CONSTRAINT stocks_pkey PRIMARY KEY (ticker);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: rxrngbplmkiody
--

REVOKE ALL ON SCHEMA public FROM postgres;
REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO rxrngbplmkiody;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: LANGUAGE plpgsql; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON LANGUAGE plpgsql TO rxrngbplmkiody;


--
-- PostgreSQL database dump complete
--

