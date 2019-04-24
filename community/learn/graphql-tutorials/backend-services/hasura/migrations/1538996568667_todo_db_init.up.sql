--
-- PostgreSQL database dump
--

-- Dumped from database version 10.5 (Debian 10.5-1.pgdg90+1)
-- Dumped by pg_dump version 10.1

-- Started on 2018-10-08 16:36:23 IST

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 213 (class 1259 OID 24585)
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE users (
    id integer NOT NULL,
    name text,
    auth0_id text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_seen timestamp with time zone DEFAULT now() NOT NULL
);


--
-- TOC entry 214 (class 1259 OID 24593)
-- Name: online_users; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW online_users AS
 SELECT users.name,
    users.last_seen
   FROM users
  WHERE (users.last_seen >= (now() - '00:00:30'::interval));


--
-- TOC entry 212 (class 1259 OID 24576)
-- Name: todos; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE todos (
    id integer NOT NULL,
    text text NOT NULL,
    is_completed boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    is_public boolean DEFAULT false NOT NULL,
    user_id text NOT NULL
);


--
-- TOC entry 215 (class 1259 OID 24597)
-- Name: todos_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE todos_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2981 (class 0 OID 0)
-- Dependencies: 215
-- Name: todos_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE todos_id_seq OWNED BY todos.id;


--
-- TOC entry 216 (class 1259 OID 24599)
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
-- TOC entry 2982 (class 0 OID 0)
-- Dependencies: 216
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- TOC entry 2839 (class 2604 OID 24612)
-- Name: todos id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY todos ALTER COLUMN id SET DEFAULT nextval('todos_id_seq'::regclass);


--
-- TOC entry 2842 (class 2604 OID 24613)
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- TOC entry 2844 (class 2606 OID 24604)
-- Name: todos todos_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY todos
    ADD CONSTRAINT todos_pkey PRIMARY KEY (id);


--
-- TOC entry 2846 (class 2606 OID 24606)
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (auth0_id);


--
-- TOC entry 2847 (class 2606 OID 24607)
-- Name: todos todos_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY todos
    ADD CONSTRAINT todos_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(auth0_id);


--
-- TOC entry 2980 (class 0 OID 0)
-- Dependencies: 4
-- Name: public; Type: ACL; Schema: -; Owner: -
--

-- Completed on 2018-10-08 16:36:25 IST

--
-- PostgreSQL database dump complete
--

