--
-- PostgreSQL database dump
--

-- Dumped from database version 10.4 (Ubuntu 10.4-2.pgdg14.04+1)
-- Dumped by pg_dump version 10.4 (Ubuntu 10.4-0ubuntu0.18.04)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: update_last_seen_timestamp_func(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_last_seen_timestamp_func() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
      BEGIN
          NEW.last_seen_at := now();
          RETURN NEW;
      END;
  $$;


SET default_tablespace = '';

SET default_with_oids = false;

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;

--
-- Name: user; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public."user" (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    online_ping boolean,
    last_seen_at timestamp with time zone
);


--
-- Name: online_users; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.online_users AS
 SELECT count(*) AS count
   FROM public."user"
  WHERE ("user".last_seen_at > (now() - '00:00:15'::interval));


--
-- Name: option; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.option (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    poll_id uuid NOT NULL,
    text text NOT NULL
);


--
-- Name: poll; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.poll (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid,
    question text NOT NULL
);


--
-- Name: vote; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.vote (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_by_user_id uuid NOT NULL,
    option_id uuid NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: poll_results; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.poll_results AS
 SELECT poll.id AS poll_id,
    o.option_id,
    count(*) AS votes
   FROM (( SELECT vote.option_id,
            option.poll_id,
            option.text
           FROM (public.vote
             LEFT JOIN public.option ON ((option.id = vote.option_id)))) o
     LEFT JOIN public.poll ON ((poll.id = o.poll_id)))
  GROUP BY poll.question, o.option_id, poll.id;


--
-- Name: option option_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.option
    ADD CONSTRAINT option_pkey PRIMARY KEY (id);


--
-- Name: poll poll_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.poll
    ADD CONSTRAINT poll_pkey PRIMARY KEY (id);


--
-- Name: user user_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: vote vote_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.vote
    ADD CONSTRAINT vote_pkey PRIMARY KEY (id);


--
-- Name: user update_last_seen_timestamp_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_last_seen_timestamp_trigger BEFORE INSERT OR UPDATE ON public."user" FOR EACH ROW EXECUTE PROCEDURE public.update_last_seen_timestamp_func();


--
-- Name: option option_poll_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.option
    ADD CONSTRAINT option_poll_id_fkey FOREIGN KEY (poll_id) REFERENCES public.poll(id);


--
-- Name: vote vote_created_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.vote
    ADD CONSTRAINT vote_created_by_user_id_fkey FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id);


--
-- Name: vote vote_option_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.vote
    ADD CONSTRAINT vote_option_id_fkey FOREIGN KEY (option_id) REFERENCES public.option(id);


--
-- PostgreSQL database dump complete
--

