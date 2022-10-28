CREATE TABLE public.message (
    id integer NOT NULL,
    username text NOT NULL,
    text text NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.message_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.message_id_seq OWNED BY public.message.id;
CREATE TABLE public."user" (
    id integer NOT NULL,
    username text NOT NULL,
    last_typed timestamp with time zone,
    last_seen timestamp with time zone
);
COMMENT ON TABLE public."user" IS 'This table stores user data';
CREATE SEQUENCE public.user_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.user_id_seq OWNED BY public."user".id;
CREATE VIEW public.user_online AS
 SELECT "user".id,
    "user".username,
    "user".last_typed,
    "user".last_seen
   FROM public."user"
  WHERE ("user".last_seen > (now() - '00:00:10'::interval));
CREATE VIEW public.user_typing AS
 SELECT "user".id,
    "user".username,
    "user".last_typed,
    "user".last_seen
   FROM public."user"
  WHERE ("user".last_typed > (now() - '00:00:02'::interval));
ALTER TABLE ONLY public.message ALTER COLUMN id SET DEFAULT nextval('public.message_id_seq'::regclass);
ALTER TABLE ONLY public."user" ALTER COLUMN id SET DEFAULT nextval('public.user_id_seq'::regclass);
ALTER TABLE ONLY public.message
    ADD CONSTRAINT message_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_username_key UNIQUE (username);
ALTER TABLE ONLY public.message
    ADD CONSTRAINT message_username_fkey FOREIGN KEY (username) REFERENCES public."user"(username);
