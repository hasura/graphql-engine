--
-- PostgreSQL database dump
--

-- Dumped from database version 10.4 (Ubuntu 10.4-2.pgdg14.04+1)
-- Dumped by pg_dump version 10.5 (Ubuntu 10.5-0ubuntu0.18.04)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Data for Name: poll; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.poll (id, created_at, created_by, question) VALUES ('98277113-a7a2-428c-9c8b-0fe7a91bf42c', '2018-08-16 12:35:44.202376+00', NULL, 'What''s your favourite frontend framework?');


--
-- Data for Name: option; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.option (id, poll_id, text) VALUES ('c44f6201-8095-421c-bfcd-b9eb8713ca84', '98277113-a7a2-428c-9c8b-0fe7a91bf42c', 'React');
INSERT INTO public.option (id, poll_id, text) VALUES ('88b78394-cdab-4b5c-a59a-0ed90c4848cf', '98277113-a7a2-428c-9c8b-0fe7a91bf42c', 'Vue');
INSERT INTO public.option (id, poll_id, text) VALUES ('1c6477bc-fb30-4eb8-b7a2-c6855da8a4f1', '98277113-a7a2-428c-9c8b-0fe7a91bf42c', 'Angular');
INSERT INTO public.option (id, poll_id, text) VALUES ('10710f03-3124-42ac-a9bd-eb8d4661657f', '98277113-a7a2-428c-9c8b-0fe7a91bf42c', 'Ember');
INSERT INTO public.option (id, poll_id, text) VALUES ('3d10e209-e906-4849-92cd-88bdd18f5b63', '98277113-a7a2-428c-9c8b-0fe7a91bf42c', 'vanilla-js.com');


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public."user" (id, created_at, online_ping, last_seen_at) VALUES ('e347b9b0-c055-e799-85c1-f9bc1105ca8c', '2018-08-29 07:01:30.156491+00', true, '2018-08-29 07:16:50.572189+00');


--
-- Data for Name: vote; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('3f49f4d9-91f7-4728-81cd-99132072b861', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', '88b78394-cdab-4b5c-a59a-0ed90c4848cf', '2018-08-29 07:01:36.312624+00');
INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('9902d8ba-c207-4679-ae8f-7f57177205a5', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', '1c6477bc-fb30-4eb8-b7a2-c6855da8a4f1', '2018-08-29 07:01:37.350155+00');
INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('a87f2898-502e-4be3-a79b-351122e0cb62', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', '3d10e209-e906-4849-92cd-88bdd18f5b63', '2018-08-29 07:01:39.419179+00');
INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('acbd82bd-9f55-450a-bbe8-1d17f10b9e4c', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', '88b78394-cdab-4b5c-a59a-0ed90c4848cf', '2018-08-29 07:01:44.209967+00');
INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('3412fc35-b1c7-4cf7-89e2-818f4dce921a', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', 'c44f6201-8095-421c-bfcd-b9eb8713ca84', '2018-08-29 07:01:34.802834+00');
INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('9d1e0552-646d-4ac1-8a3c-4493dd9eea1a', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', '10710f03-3124-42ac-a9bd-eb8d4661657f', '2018-08-29 07:01:38.55102+00');
INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('8ce344a1-056f-4e50-8c6b-23aafb8ec0ce', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', 'c44f6201-8095-421c-bfcd-b9eb8713ca84', '2018-08-29 07:01:41.898396+00');
INSERT INTO public.vote (id, created_by_user_id, option_id, created_at) VALUES ('86a5d415-df1f-42b0-9dce-3c2b8d31c3a0', 'e347b9b0-c055-e799-85c1-f9bc1105ca8c', '88b78394-cdab-4b5c-a59a-0ed90c4848cf', '2018-08-29 07:01:43.785034+00');


--
-- PostgreSQL database dump complete
--

