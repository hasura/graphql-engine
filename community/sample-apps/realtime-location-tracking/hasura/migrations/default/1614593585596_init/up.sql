CREATE TABLE public.vehicle (
    id text NOT NULL,
    name text NOT NULL
);
CREATE SEQUENCE public.vehicle_loc
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
CREATE TABLE public.vehicle_location (
    id integer DEFAULT nextval('public.vehicle_loc'::regclass) NOT NULL,
    vehicle_id text NOT NULL,
    location text NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE ONLY public.vehicle_location
    ADD CONSTRAINT vehicle_location_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.vehicle
    ADD CONSTRAINT vehicle_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.vehicle_location
    ADD CONSTRAINT vehicle_location_vehicle_id_fkey FOREIGN KEY (vehicle_id) REFERENCES public.vehicle(id);
