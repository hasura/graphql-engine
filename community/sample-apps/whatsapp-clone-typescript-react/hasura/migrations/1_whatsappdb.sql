--
-- PostgreSQL database dump
--

-- Dumped from database version 10.5 (Debian 10.5-1.pgdg90+1)
-- Dumped by pg_dump version 10.1

-- Started on 2019-03-11 11:42:52 IST

-- TOC entry 285 (class 1255 OID 24760)
-- Name: truncate_tables(character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION truncate_tables(username character varying) RETURNS void
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


SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 216 (class 1259 OID 16585)
-- Name: chat; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE chat (
    id integer NOT NULL,
    name text,
    picture text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    owner_id integer
);


--
-- TOC entry 3059 (class 0 OID 0)
-- Dependencies: 216
-- Name: TABLE chat; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE chat IS 'chats having an owner is a group';


--
-- TOC entry 3060 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN chat.owner_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN chat.owner_id IS 'If owner_id is present, its a group chat';


--
-- TOC entry 217 (class 1259 OID 16592)
-- Name: chat_group_admins; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE chat_group_admins (
    chat_id integer NOT NULL,
    user_id integer NOT NULL
);


--
-- TOC entry 3061 (class 0 OID 0)
-- Dependencies: 217
-- Name: TABLE chat_group_admins; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE chat_group_admins IS 'chat group admin mapping';


--
-- TOC entry 219 (class 1259 OID 16597)
-- Name: chat_users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE chat_users (
    chat_id integer NOT NULL,
    user_id integer NOT NULL
);


--
-- TOC entry 3062 (class 0 OID 0)
-- Dependencies: 219
-- Name: TABLE chat_users; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE chat_users IS 'chat user mapping';


--
-- TOC entry 220 (class 1259 OID 16608)
-- Name: message; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE message (
    id integer NOT NULL,
    content text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    sender_id integer,
    chat_id integer
);


--
-- TOC entry 222 (class 1259 OID 16617)
-- Name: recipient; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE recipient (
    id integer NOT NULL,
    received_at timestamp with time zone,
    read_at timestamp with time zone,
    user_id integer NOT NULL,
    message_id integer NOT NULL
);


--
-- TOC entry 224 (class 1259 OID 16622)
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE users (
    id integer NOT NULL,
    username text NOT NULL,
    password text NOT NULL,
    name text DEFAULT ''''::text,
    picture text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- TOC entry 218 (class 1259 OID 16595)
-- Name: chat_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE chat_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 3063 (class 0 OID 0)
-- Dependencies: 218
-- Name: chat_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE chat_id_seq OWNED BY chat.id;


--
-- TOC entry 221 (class 1259 OID 16615)
-- Name: message_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE message_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 3064 (class 0 OID 0)
-- Dependencies: 221
-- Name: message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE message_id_seq OWNED BY message.id;


--
-- TOC entry 226 (class 1259 OID 32940)
-- Name: message_user; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW message_user AS
 SELECT message.id,
    message.content,
    message.created_at,
    message.sender_id,
    message.chat_id
   FROM message
  ORDER BY message.id DESC;


--
-- TOC entry 223 (class 1259 OID 16620)
-- Name: recipient_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE recipient_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 3065 (class 0 OID 0)
-- Dependencies: 223
-- Name: recipient_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE recipient_id_seq OWNED BY recipient.id;


--
-- TOC entry 225 (class 1259 OID 16630)
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 3066 (class 0 OID 0)
-- Dependencies: 225
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- TOC entry 2892 (class 2604 OID 16632)
-- Name: chat id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat ALTER COLUMN id SET DEFAULT nextval('chat_id_seq'::regclass);


--
-- TOC entry 2894 (class 2604 OID 16634)
-- Name: message id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY message ALTER COLUMN id SET DEFAULT nextval('message_id_seq'::regclass);


--
-- TOC entry 2895 (class 2604 OID 16635)
-- Name: recipient id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY recipient ALTER COLUMN id SET DEFAULT nextval('recipient_id_seq'::regclass);


--
-- TOC entry 2898 (class 2604 OID 16636)
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- TOC entry 2902 (class 2606 OID 16638)
-- Name: chat_group_admins chat_group_admins_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat_group_admins
    ADD CONSTRAINT chat_group_admins_pkey PRIMARY KEY (chat_id, user_id);


--
-- TOC entry 2900 (class 2606 OID 16640)
-- Name: chat chat_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat
    ADD CONSTRAINT chat_pkey PRIMARY KEY (id);


--
-- TOC entry 2904 (class 2606 OID 16642)
-- Name: chat_users chat_users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat_users
    ADD CONSTRAINT chat_users_pkey PRIMARY KEY (chat_id, user_id);


--
-- TOC entry 2906 (class 2606 OID 16646)
-- Name: message message_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_pkey PRIMARY KEY (id);


--
-- TOC entry 2908 (class 2606 OID 16648)
-- Name: recipient recipient_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY recipient
    ADD CONSTRAINT recipient_pkey PRIMARY KEY (user_id, message_id);


--
-- TOC entry 2910 (class 2606 OID 16650)
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- TOC entry 2912 (class 2606 OID 16651)
-- Name: chat_group_admins chat_group_admins_chat_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat_group_admins
    ADD CONSTRAINT chat_group_admins_chat_id_fkey FOREIGN KEY (chat_id) REFERENCES chat(id);


--
-- TOC entry 2913 (class 2606 OID 16656)
-- Name: chat_group_admins chat_group_admins_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat_group_admins
    ADD CONSTRAINT chat_group_admins_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- TOC entry 2911 (class 2606 OID 16661)
-- Name: chat chat_owner_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat
    ADD CONSTRAINT chat_owner_id_fkey FOREIGN KEY (owner_id) REFERENCES users(id);


--
-- TOC entry 2914 (class 2606 OID 16666)
-- Name: chat_users chat_users_chat_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat_users
    ADD CONSTRAINT chat_users_chat_id_fkey FOREIGN KEY (chat_id) REFERENCES chat(id);


--
-- TOC entry 2915 (class 2606 OID 16671)
-- Name: chat_users chat_users_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY chat_users
    ADD CONSTRAINT chat_users_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- TOC entry 2916 (class 2606 OID 16676)
-- Name: message message_chat_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_chat_id_fkey FOREIGN KEY (chat_id) REFERENCES chat(id);


--
-- TOC entry 2917 (class 2606 OID 16681)
-- Name: message message_sender_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_sender_id_fkey FOREIGN KEY (sender_id) REFERENCES users(id);


--
-- TOC entry 2918 (class 2606 OID 16686)
-- Name: recipient recipient_message_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY recipient
    ADD CONSTRAINT recipient_message_id_fkey FOREIGN KEY (message_id) REFERENCES message(id);


--
-- TOC entry 2919 (class 2606 OID 16691)
-- Name: recipient recipient_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY recipient
    ADD CONSTRAINT recipient_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


-- Completed on 2019-03-11 11:42:53 IST

--
-- PostgreSQL database dump complete
--

