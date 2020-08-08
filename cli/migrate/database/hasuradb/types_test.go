package hasuradb

import (
	"encoding/json"
	"testing"
)

func TestHasuraError_Error(t *testing.T) {
	type fields struct {
		migrationFile  string
		migrationQuery string
		Path           string
		ErrorMessage   string
		Internal       interface{}
		Message        string
		Code           string
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can unmarshal internal error",
			fields{
				Internal: func() interface{} {
					d := []byte(`
{
"error": {
	"status_code": "1"
}
}`)
					var v interface{}
					json.Unmarshal(d, &v)
					return v
				}(),
			},
			"[]  ()\r\n[1] : ",
		},
		{
			"can unmarshal internal errors",
			fields{
				Internal: func() interface{} {
					d := []byte(`
[{
"error": {
	"status_code": "2"
}
}]`)
					var v interface{}
					json.Unmarshal(d, &v)
					return v
				}(),
			},
			"[]  ()\r\n[2] : ",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := HasuraError{
				migrationFile:  tt.fields.migrationFile,
				migrationQuery: tt.fields.migrationQuery,
				Path:           tt.fields.Path,
				ErrorMessage:   tt.fields.ErrorMessage,
				Internal:       tt.fields.Internal,
				Message:        tt.fields.Message,
				Code:           tt.fields.Code,
			}
			if got := h.Error(); got != tt.want {
				t.Errorf("Error() = %v, want %v", got, tt.want)
			}
		})
	}
}
