SET check_function_bodies = false;
INSERT INTO public.tag (id, value) VALUES (1, 't1');
INSERT INTO public.tag (id, value) VALUES (2, 't2');
INSERT INTO public.tag (id, value) VALUES (3, 't3');
SELECT pg_catalog.setval('public.tag_id_seq', 3, true);
