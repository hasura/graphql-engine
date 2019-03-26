CREATE TABLE public.articles (
    id integer NOT NULL,
    title text NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    length integer NOT NULL
);


CREATE SEQUENCE public.article_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


CREATE TABLE public.comments (
    uuid uuid DEFAULT public.gen_random_uuid() NOT NULL,
    article_id integer NOT NULL,
    comment text NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);



CREATE TABLE public.likes (
    uuid uuid DEFAULT public.gen_random_uuid() NOT NULL,
    article_id integer NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);



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



CREATE TABLE public.stocks (
    ticker text NOT NULL,
    price numeric NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE ONLY public.articles ALTER COLUMN id SET DEFAULT nextval('public.article_id_seq'::regclass);

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT article_pkey PRIMARY KEY (id);


ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (uuid);


ALTER TABLE ONLY public.likes
    ADD CONSTRAINT likes_pkey PRIMARY KEY (uuid);

ALTER TABLE ONLY public.stocks
    ADD CONSTRAINT stocks_pkey PRIMARY KEY (ticker);

