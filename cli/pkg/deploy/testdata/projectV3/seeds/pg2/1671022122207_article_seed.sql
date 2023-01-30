SET check_function_bodies = false;
INSERT INTO public.article (id, title) VALUES (1, 'a1');
INSERT INTO public.article (id, title) VALUES (2, 'a2');
SELECT pg_catalog.setval('public.article_id_seq', 2, true);
