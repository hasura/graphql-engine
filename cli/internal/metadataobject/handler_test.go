package metadataobject

import (
	"encoding/json"
	"testing"
)

func Test_inconsistentObject_GetName(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can decode without a name",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": "article",
        "reason": "no such table/view exists in postgres : \"article\"",
        "type": "table"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"N/A",
		},
		{
			"can decode a with a name",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": {
                    "column": "author_id",
                    "table": "article"
                }
            },
            "name": "articles",
            "comment": null,
            "table": "author"
        },
        "reason": "table \"article\" does not exist",
        "type": "array_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"articles",
		},
		{
			"can decode a with a name",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"author",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetName(); got != tt.want {
				t.Errorf("GetName() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_inconsistentObject_GetDescription(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can generate description",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`{"comment":null,"name":"author","table":"article",...`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetDescription(); got != tt.want {
				t.Errorf("GetDescription() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_inconsistentObject_GetType(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can get type",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": "article",
        "reason": "no such table/view exists in postgres : \"article\"",
        "type": "table"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"table",
		},
		{
			"can get type",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": {
                    "column": "author_id",
                    "table": "article"
                }
            },
            "name": "articles",
            "comment": null,
            "table": "author"
        },
        "reason": "table \"article\" does not exist",
        "type": "array_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"array_relation",
		},
		{
			"can get type",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"object_relation",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetType(); got != tt.want {
				t.Errorf("GetType() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_inconsistentObject_GetReason(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can get reason",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": "article",
        "reason": "no such table/view exists in postgres : \"article\"",
        "type": "table"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`no such table/view exists in postgres : "article"`,
		},
		{
			"can get reason",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": {
                    "column": "author_id",
                    "table": "article"
                }
            },
            "name": "articles",
            "comment": null,
            "table": "author"
        },
        "reason": "table \"article\" does not exist",
        "type": "array_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`table "article" does not exist`,
		},
		{
			"can get reason",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`table "article" does not exist`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetReason(); got != tt.want {
				t.Errorf("GetReason() = %v, want %v", got, tt.want)
			}
		})
	}
}
