CREATE TABLE public.message (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL,
    title text NOT NULL,
    body text,
    device_token text NOT NULL
);
ALTER TABLE ONLY public.message
    ADD CONSTRAINT message_pkey PRIMARY KEY (id);
