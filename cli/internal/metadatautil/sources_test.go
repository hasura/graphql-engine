package metadatautil

import (
	"bytes"
	"errors"
	"io"
	"io/ioutil"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var kindblackhole *hasura.SourceKind

func BenchmarkGetSourceKind(b *testing.B) {
	funcs := []struct {
		name string
		f    func(func() (io.Reader, error), string) (*hasura.SourceKind, error)
	}{
		{"jsonparser", GetSourceKind},
	}
	for _, f := range funcs {
		b.Run(f.name, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				input, err := ioutil.ReadFile("testdata/json/t2/metadata.json")
				assert.NoError(b, err)
				kindblackhole, err = f.f(func() (io.Reader, error) { return bytes.NewReader(input), nil }, "default")
				assert.NoError(b, err)
			}
		})
	}
}

func TestGetSourceKind(t *testing.T) {
	type args struct {
		exportMetadata func() (io.Reader, error)
		sourceName     string
	}
	tests := []struct {
		name      string
		args      args
		want      hasura.SourceKind
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can find source kind",
			args{
				exportMetadata: func() (io.Reader, error) {
					s := `
{
	"sources": [
		{
			"name": "test",
			"kind": "postgres"
		},
		{
			"name": "test2",
			"kind": "mssql"
		}
	]
}
`
					return strings.NewReader(s), nil
				},
				sourceName: "test",
			},
			hasura.SourceKindPG,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetSourceKind(tt.args.exportMetadata, tt.args.sourceName)
			tt.assertErr(t, err)
			if !tt.wantErr {
				assert.NotNil(t, got)
				assert.Equal(t, tt.want, *got)
			}
		})
	}
}

func TestGetSources(t *testing.T) {
	type args struct {
		exportMetadata func() (io.Reader, error)
	}
	tests := []struct {
		name      string
		args      args
		want      []string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can get list of sources",
			args{
				func() (io.Reader, error) {
					return strings.NewReader(
						`
{
	"sources": [
		{
			"name": "test1"
		},
		{

			"name": "test2"
		}
	]
}
`), nil
				},
			},
			[]string{"test1", "test2"},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetSources(tt.args.exportMetadata)
			tt.assertErr(t, err)
			if !tt.wantErr {
				require.Equal(t, got, tt.want)
			}
		})
	}
}

func TestGetSourcesAndKind(t *testing.T) {
	type args struct {
		exportMetadata func() (io.Reader, error)
	}
	tests := []struct {
		name      string
		args      args
		want      []Source
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can get sources and kind",
			args{
				func() (io.Reader, error) {
					return strings.NewReader(
						`
{
	"sources": [
		{
			"name": "test1",
			"kind": "postgres"
		},
		{

			"name": "test2",
			"kind": "mssql"
		}
	]
}
`), nil
				},
			},
			[]Source{{"test1", hasura.SourceKindPG}, {"test2", hasura.SourceKindMSSQL}},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetSourcesAndKind(tt.args.exportMetadata)
			tt.assertErr(t, err)
			if !tt.wantErr {
				require.Equal(t, got, tt.want)
			}
		})
	}
}

func TestGetSourcesAndKindStrict(t *testing.T) {
	type args struct {
		exportMetadata func() (io.Reader, error)
	}
	tests := []struct {
		name      string
		args      args
		want      []Source
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can return right error type when no sources are connected",
			args{
				exportMetadata: func() (io.Reader, error) {
					metadata := `
{
	"sources": [],
}
					`
					return strings.NewReader(metadata), nil
				},
			},
			nil,
			true,
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.Truef(t, errors.Is(err, ErrNoConnectedSources), "expected to find ErrNoConnectedSources in error chain")
			}),
		},
		{
			"can parse sources when they are present",
			args{
				exportMetadata: func() (io.Reader, error) {
					metadata := `
{
	"sources": [
		{
			"name": "something",
			"kind": "postgres"
		}
	],
}
					`
					return strings.NewReader(metadata), nil
				},
			},
			[]Source{{Name: "something", Kind: "postgres"}},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetSourcesAndKindStrict(tt.args.exportMetadata)
			tt.assertErr(t, err)
			if !tt.wantErr {
				assert.Equal(t, tt.want, got)
			}
		})
	}
}
