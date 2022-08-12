CREATE TABLE public.book (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    title text NOT NULL,
    author text NOT NULL
);
ALTER TABLE ONLY public.book
    ADD CONSTRAINT book_pkey PRIMARY KEY (id);

INSERT INTO public.book (id, created_at, title, author) VALUES ('b9a89c23-fbbf-4250-8dae-aecd1f5025b9', '2021-03-01 07:03:26.674988+00', 'The Davinci Code', 'Dan Brown');
INSERT INTO public.book (id, created_at, title, author) VALUES ('55feca43-73f4-4592-8c10-328ca2b1212b', '2021-03-01 07:08:06.867025+00', 'India After Gandhi', 'Ramachandra Guha');
INSERT INTO public.book (id, created_at, title, author) VALUES ('dae05a21-d153-445d-980c-1429eec21556', '2021-03-01 07:24:36.713387+00', 'Antifragile', 'Taleb');
INSERT INTO public.book (id, created_at, title, author) VALUES ('c27bafd2-ba97-4c90-b213-451f8864af39', '2021-03-01 07:26:02.34931+00', 'Midnight Children', 'Salman Rushdi');
INSERT INTO public.book (id, created_at, title, author) VALUES ('2f4b1a96-587e-45be-b025-f16847915701', '2021-03-01 07:37:31.639083+00', 'Harry Potter', 'JK Rowling');
INSERT INTO public.book (id, created_at, title, author) VALUES ('fadb38ab-8783-4b46-b857-a9bb2eba7a54', '2021-03-01 07:56:46.103464+00', 'To Kill a Mockingbird', 'Harper Lee');
INSERT INTO public.book (id, created_at, title, author) VALUES ('2880faa6-beba-4700-a43f-edda50088e7e', '2021-03-01 08:46:08.883573+00', 'Oliver Twist', 'Charles Dickens');
INSERT INTO public.book (id, created_at, title, author) VALUES ('91d06688-8fcc-4784-a64e-90c3aa71e8bc', '2021-03-01 11:05:01.423099+00', 'The Experiments of Life', 'Mahatma Gandhi');
INSERT INTO public.book (id, created_at, title, author) VALUES ('4bd58a98-552e-4622-9af3-d75a6fbbcce2', '2021-03-01 11:07:13.930607+00', 'Robinson Crusoe', 'Daniel Defoe');
INSERT INTO public.book (id, created_at, title, author) VALUES ('4e1f75fc-b1ba-46f4-ab89-243a5d6db61d', '2021-03-01 11:11:46.157602+00', ' Tom Jones', 'Henry Fielding');
INSERT INTO public.book (id, created_at, title, author) VALUES ('60517455-65be-475e-a768-fcb4f67cc822', '2021-03-01 11:12:17.803557+00', 'Clarissa', 'Samuel Richardson');
INSERT INTO public.book (id, created_at, title, author) VALUES ('ca98f6a0-b525-4081-84e9-d606336661bb', '2021-03-01 11:25:47.881804+00', 'Great Expectations', 'Charles Dickens');
INSERT INTO public.book (id, created_at, title, author) VALUES ('f64ef42a-e883-41e5-b528-61824d26faee', '2021-03-01 12:19:29.955485+00', 'Jungle Book', 'Rudyard Kipling');
INSERT INTO public.book (id, created_at, title, author) VALUES ('155f4aba-a68b-4769-b41f-a94b81e57cad', '2021-03-01 13:11:23.215933+00', 'Antifragile', 'Taleb');
INSERT INTO public.book (id, created_at, title, author) VALUES ('3acea9a1-8a2b-400a-86d1-bde35e729e91', '2021-03-01 16:49:05.813989+00', 'Teste', 'Miguel');
