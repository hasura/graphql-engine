package seed

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestCleanDataDump(t *testing.T) {
	tests := []struct {
		name string
		dump string
		want string
	}{
		{
			"drops preamble, comments and blank lines like clean_output",
			`--
-- PostgreSQL database dump
--

\restrict hasura
SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;

--
-- Data for Name: authors; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.authors (id, name) VALUES (1, 'test1');
INSERT INTO public.authors (id, name) VALUES (4, 'it''s');


SELECT pg_catalog.setval('public.authors_id_seq', 1, false);

\unrestrict hasura
`,
			`\restrict hasura
SET check_function_bodies = false;
INSERT INTO public.authors (id, name) VALUES (1, 'test1');
INSERT INTO public.authors (id, name) VALUES (4, 'it''s');
SELECT pg_catalog.setval('public.authors_id_seq', 1, false);
\unrestrict hasura
`,
		},
		{
			"keeps blank lines and -- lines inside multi-line text",
			`SET statement_timeout = 0;

INSERT INTO public.notes (id, body) VALUES (1, 'first paragraph

-- not a comment
second paragraph with a quote: ''

');
INSERT INTO public.notes (id, body) VALUES (2, 'single line');
`,
			`INSERT INTO public.notes (id, body) VALUES (1, 'first paragraph

-- not a comment
second paragraph with a quote: ''

');
INSERT INTO public.notes (id, body) VALUES (2, 'single line');
`,
		},
		{
			"a single quote inside a quoted identifier does not open a string",
			`INSERT INTO public."it's" (id) VALUES (1);

INSERT INTO public."it's" (id) VALUES (2);
`,
			`INSERT INTO public."it's" (id) VALUES (1);
INSERT INTO public."it's" (id) VALUES (2);
`,
		},
		{
			"empty dump",
			"",
			"",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			require.Equal(t, tc.want, cleanDataDump(tc.dump))
		})
	}
}
