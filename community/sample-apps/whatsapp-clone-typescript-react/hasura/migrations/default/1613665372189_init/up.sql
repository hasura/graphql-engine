CREATE FUNCTION public.truncate_tables(username character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
    statements CURSOR FOR
        SELECT tablename FROM pg_tables
        WHERE tableowner = username AND schemaname = 'public';
BEGIN
    FOR stmt IN statements LOOP
        EXECUTE 'TRUNCATE TABLE ' || quote_ident(stmt.tablename) || ' CASCADE;';
    END LOOP;
END;
$$;
CREATE TABLE public.chat (
    id integer NOT NULL,
    name text,
    picture text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    owner_id integer
);
COMMENT ON TABLE public.chat IS 'chats having an owner is a group';
COMMENT ON COLUMN public.chat.owner_id IS 'If owner_id is present, its a group chat';
CREATE TABLE public.chat_group_admins (
    chat_id integer NOT NULL,
    user_id integer NOT NULL
);
COMMENT ON TABLE public.chat_group_admins IS 'chat group admin mapping';
CREATE SEQUENCE public.chat_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.chat_id_seq OWNED BY public.chat.id;
CREATE TABLE public.chat_users (
    chat_id integer NOT NULL,
    user_id integer NOT NULL
);
COMMENT ON TABLE public.chat_users IS 'chat user mapping';
CREATE TABLE public.message (
    id integer NOT NULL,
    content text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    sender_id integer,
    chat_id integer
);
CREATE SEQUENCE public.message_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.message_id_seq OWNED BY public.message.id;
CREATE VIEW public.message_user AS
 SELECT message.id,
    message.content,
    message.created_at,
    message.sender_id,
    message.chat_id
   FROM public.message
  ORDER BY message.id DESC;
CREATE TABLE public.recipient (
    id integer NOT NULL,
    received_at timestamp with time zone,
    read_at timestamp with time zone,
    user_id integer NOT NULL,
    message_id integer NOT NULL
);
CREATE SEQUENCE public.recipient_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.recipient_id_seq OWNED BY public.recipient.id;
CREATE TABLE public.users (
    id integer NOT NULL,
    username text NOT NULL,
    password text NOT NULL,
    name text DEFAULT ''''::text,
    picture text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;
ALTER TABLE ONLY public.chat ALTER COLUMN id SET DEFAULT nextval('public.chat_id_seq'::regclass);
ALTER TABLE ONLY public.message ALTER COLUMN id SET DEFAULT nextval('public.message_id_seq'::regclass);
ALTER TABLE ONLY public.recipient ALTER COLUMN id SET DEFAULT nextval('public.recipient_id_seq'::regclass);
ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);
ALTER TABLE ONLY public.chat_group_admins
    ADD CONSTRAINT chat_group_admins_pkey PRIMARY KEY (chat_id, user_id);
ALTER TABLE ONLY public.chat
    ADD CONSTRAINT chat_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.chat_users
    ADD CONSTRAINT chat_users_pkey PRIMARY KEY (chat_id, user_id);
ALTER TABLE ONLY public.message
    ADD CONSTRAINT message_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.recipient
    ADD CONSTRAINT recipient_pkey PRIMARY KEY (user_id, message_id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.chat_group_admins
    ADD CONSTRAINT chat_group_admins_chat_id_fkey FOREIGN KEY (chat_id) REFERENCES public.chat(id);
ALTER TABLE ONLY public.chat_group_admins
    ADD CONSTRAINT chat_group_admins_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);
ALTER TABLE ONLY public.chat
    ADD CONSTRAINT chat_owner_id_fkey FOREIGN KEY (owner_id) REFERENCES public.users(id);
ALTER TABLE ONLY public.chat_users
    ADD CONSTRAINT chat_users_chat_id_fkey FOREIGN KEY (chat_id) REFERENCES public.chat(id);
ALTER TABLE ONLY public.chat_users
    ADD CONSTRAINT chat_users_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);
ALTER TABLE ONLY public.message
    ADD CONSTRAINT message_chat_id_fkey FOREIGN KEY (chat_id) REFERENCES public.chat(id);
ALTER TABLE ONLY public.message
    ADD CONSTRAINT message_sender_id_fkey FOREIGN KEY (sender_id) REFERENCES public.users(id);
ALTER TABLE ONLY public.recipient
    ADD CONSTRAINT recipient_message_id_fkey FOREIGN KEY (message_id) REFERENCES public.message(id);
ALTER TABLE ONLY public.recipient
    ADD CONSTRAINT recipient_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);
