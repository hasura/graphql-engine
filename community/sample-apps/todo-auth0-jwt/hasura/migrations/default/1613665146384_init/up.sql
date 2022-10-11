CREATE TABLE public.todo (
    id integer NOT NULL,
    task text NOT NULL,
    completed boolean NOT NULL,
    user_id text NOT NULL
);
CREATE SEQUENCE public.todo_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.todo_id_seq OWNED BY public.todo.id;
ALTER TABLE ONLY public.todo ALTER COLUMN id SET DEFAULT nextval('public.todo_id_seq'::regclass);
ALTER TABLE ONLY public.todo
    ADD CONSTRAINT todo_pkey PRIMARY KEY (id);
