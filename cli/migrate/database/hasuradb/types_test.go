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

func TestInconsistentMetadataError_String(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     string
		Type       string
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can generate error correctly when all fields are given",
			fields{
				Reason: "test reason",
				Type:   "test",
				Definition: func() interface{} {
					var m interface{}
					err := json.Unmarshal([]byte(`{"test": "test"}`), &m)
					if err != nil {
						t.Error(err)
					}
					return m
				}(),
			},
			`
reason: test reason
type: test
definition: 
{
  "test": "test"
}`,
		},
		{
			"will not panic when Definition is not a valid json (string)",
			fields{
				Definition: func() interface{} {
					return "test"
				}(),
				Reason: "",
				Type:   "",
			},
			`definition: 
"test"`,
		},
		{
			"will not panic when Definition is not a valid json (Int)",
			fields{
				Definition: func() interface{} {
					return 1
				}(),
				Reason: "",
				Type:   "",
			},
			`definition: 
1`,
		},
		{
			"will not panic when Definition is (struct Array)",
			fields{
				Definition: func() interface{} {
					return []struct{ Name string }{{"test"}, {"test"}}
				}(),
				Reason: "",
				Type:   "",
			},
			`definition: 
[
  {
    "Name": "test"
  },
  {
    "Name": "test"
  }
]`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mderror := &InconsistentMetadataError{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := mderror.String(); got != tt.want {
				t.Errorf("String() = %v, want %v", got, tt.want)
			}
		})
	}
}
