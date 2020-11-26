package hasuradb

import (
	"net/http"
	"testing"

	"github.com/stretchr/testify/assert"
)

type mockCatalogAPI struct {
	mock metadataOrQueryClientFunc
}

func (c *mockCatalogAPI) sendMetadataOrQueryRequest(m interface{}, queryType string) (*http.Response, []byte, error) {
	return c.mock(m, queryType)
}

func TestCatalogStateAPI_GetCLICatalogState(t *testing.T) {
	type fields struct {
		CLIStateKeyName string
	}
	type args struct {
		client CatalogStateAPIClient
	}
	mockClient := new(mockCatalogAPI)
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *CLICatalogState
		wantErr bool
	}{
		{
			"can pass",
			fields{
				"cli_state",
			},
			args{
				func() CatalogStateAPIClient {
					mockClient.mock = func(m interface{}, queryType string) (*http.Response, []byte, error) {
						r := &http.Response{
							StatusCode: http.StatusOK,
						}
						body := []byte(`
{
    "id": "7929d6df-b82f-4ba3-b9ef-72fd48d337d9",
    "cli_state": {
        "settings": {
			"test": "test"
		},
        "migrations": {
            "default": {
                "1606208931894": false
            }
        }
    },
    "console_state": {}
}
`)
						return r, body, nil
					}
					return mockClient
				}(),
			},
			&CLICatalogState{
				Migrations: MigrationsState{
					"default": map[string]bool{
						"1606208931894": false,
					},
				},
				Settings: map[string]string{
					"test": "test",
				},
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &CatalogStateAPI{
				CLIStateKeyName: tt.fields.CLIStateKeyName,
			}
			got, err := c.GetCLICatalogState(tt.args.client)
			if (err != nil) != tt.wantErr {
				t.Errorf("GetCLICatalogState() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			assert.Equal(t, tt.want, got)
		})
	}
}
