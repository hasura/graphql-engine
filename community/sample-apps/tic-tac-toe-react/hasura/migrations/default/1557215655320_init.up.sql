CREATE TABLE public.board (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    user_1_id integer NOT NULL,
    user_2_id integer,
    winner text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    turn text NOT NULL
);
CREATE TABLE public.move (
    id integer NOT NULL,
    "position" integer NOT NULL,
    user_id integer NOT NULL,
    board_id uuid NOT NULL
);
CREATE SEQUENCE public.move_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.move_id_seq OWNED BY public.move.id;
CREATE TABLE public."user" (
    id integer NOT NULL,
    name text NOT NULL,
    last_seen timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.user_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.user_id_seq OWNED BY public."user".id;
ALTER TABLE ONLY public.move ALTER COLUMN id SET DEFAULT nextval('public.move_id_seq'::regclass);
ALTER TABLE ONLY public."user" ALTER COLUMN id SET DEFAULT nextval('public.user_id_seq'::regclass);
ALTER TABLE ONLY public.board
    ADD CONSTRAINT board_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.move
    ADD CONSTRAINT move_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.board
    ADD CONSTRAINT board_user_1_id_fkey FOREIGN KEY (user_1_id) REFERENCES public."user"(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.board
    ADD CONSTRAINT board_user_2_id_fkey FOREIGN KEY (user_2_id) REFERENCES public."user"(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.move
    ADD CONSTRAINT move_board_id_fkey FOREIGN KEY (board_id) REFERENCES public.board(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.move
    ADD CONSTRAINT move_user_id_fkey FOREIGN KEY (user_id) REFERENCES public."user"(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
