package metadatautil

import (
	"io"
	"reflect"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/stretchr/testify/assert"
)

func TestGetSourceKind(t *testing.T) {
	type args struct {
		exportMetadata func() (io.Reader, error)
		sourceName     string
	}
	tests := []struct {
		name    string
		args    args
		want    hasura.SourceKind
		wantErr bool
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
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetSourceKind(tt.args.exportMetadata, tt.args.sourceName)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
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
		name    string
		args    args
		want    []string
		wantErr bool
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
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetSources(tt.args.exportMetadata)
			if (err != nil) != tt.wantErr {
				t.Errorf("GetSources() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("GetSources() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetSourcesAndKind(t *testing.T) {
	type args struct {
		exportMetadata func() (io.Reader, error)
	}
	tests := []struct {
		name    string
		args    args
		want    []Source
		wantErr bool
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
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetSourcesAndKind(tt.args.exportMetadata)
			if (err != nil) != tt.wantErr {
				t.Errorf("GetSourcesAndKind() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("GetSourcesAndKind() got = %v, want %v", got, tt.want)
			}
		})
	}
}
