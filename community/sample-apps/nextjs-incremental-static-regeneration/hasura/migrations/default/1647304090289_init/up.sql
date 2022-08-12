SET check_function_bodies = false;
CREATE TABLE public.post (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title text NOT NULL,
    content text NOT NULL
);
ALTER TABLE ONLY public.post
    ADD CONSTRAINT blog_pkey PRIMARY KEY (id);
