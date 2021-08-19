CREATE TABLE public.article (
    id integer NOT NULL,
    title text NOT NULL,
    user_id text NOT NULL
);
CREATE SEQUENCE public.article_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.article_id_seq OWNED BY public.article.id;
CREATE TABLE public.users (
    auth0_id text NOT NULL,
    name text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE ONLY public.article ALTER COLUMN id SET DEFAULT nextval('public.article_id_seq'::regclass);
ALTER TABLE ONLY public.article
    ADD CONSTRAINT article_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (auth0_id);
ALTER TABLE ONLY public.article
    ADD CONSTRAINT article_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(auth0_id);
