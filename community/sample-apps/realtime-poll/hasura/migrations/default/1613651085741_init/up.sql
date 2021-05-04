CREATE FUNCTION public.update_last_seen_timestamp_func() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
      BEGIN
          NEW.last_seen_at := now();
          RETURN NEW;
      END;
  $$;
CREATE TABLE public."user" (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    online_ping boolean,
    last_seen_at timestamp with time zone
);
CREATE VIEW public.online_users AS
 SELECT count(*) AS count
   FROM public."user"
  WHERE ("user".last_seen_at > (now() - '00:00:15'::interval));
CREATE TABLE public.option (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    poll_id uuid NOT NULL,
    text text NOT NULL
);
CREATE TABLE public.poll (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid,
    question text NOT NULL
);
CREATE TABLE public.vote (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_by_user_id uuid NOT NULL,
    option_id uuid NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
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
ALTER TABLE ONLY public.option
    ADD CONSTRAINT option_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.poll
    ADD CONSTRAINT poll_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.vote
    ADD CONSTRAINT vote_pkey PRIMARY KEY (id);
CREATE TRIGGER update_last_seen_timestamp_trigger BEFORE INSERT OR UPDATE ON public."user" FOR EACH ROW EXECUTE FUNCTION public.update_last_seen_timestamp_func();
ALTER TABLE ONLY public.option
    ADD CONSTRAINT option_poll_id_fkey FOREIGN KEY (poll_id) REFERENCES public.poll(id);
ALTER TABLE ONLY public.vote
    ADD CONSTRAINT vote_created_by_user_id_fkey FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id);
ALTER TABLE ONLY public.vote
    ADD CONSTRAINT vote_option_id_fkey FOREIGN KEY (option_id) REFERENCES public.option(id);

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