package commands

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v2query"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	yaml "go.yaml.in/yaml/v3"
)

func parseTableNode(t *testing.T, src string) *yaml.Node {
	t.Helper()
	var doc yaml.Node
	require.NoError(t, yaml.Unmarshal([]byte(src), &doc))
	return documentRoot(&doc)
}

func encodeNode(t *testing.T, node *yaml.Node) string {
	t.Helper()
	data, err := encodeYAMLDocument(node)
	require.NoError(t, err)
	return string(data)
}

func tableOnly(tables map[tableKey]tableComments) *sourceComments {
	return &sourceComments{tables: tables, functions: map[tableKey]funcComment{}}
}

// planOne applies the given table comments to a single parsed table node.
func planOne(t *testing.T, src string, tc tableComments) (*tableCommentChange, *yaml.Node, error) {
	t.Helper()
	root := parseTableNode(t, src)
	schema, table, ok := tableIdentifier(root)
	require.True(t, ok)
	change, err := planTableNode(
		"default",
		"test.yaml",
		root,
		tableOnly(map[tableKey]tableComments{{schema: schema, table: table}: tc}),
	)
	return change, root, err
}

func TestTableIdentifier(t *testing.T) {
	cases := []struct {
		name       string
		yaml       string
		wantSchema string
		wantTable  string
		wantOK     bool
	}{
		{
			"mapping with schema",
			"table:\n  schema: myschema\n  name: orders\n",
			"myschema",
			"orders",
			true,
		},
		{"mapping default schema", "table:\n  name: orders\n", "public", "orders", true},
		{"bare string", "table: orders\n", "public", "orders", true},
		{"missing table", "configuration: {}\n", "", "", false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			schema, table, ok := tableIdentifier(parseTableNode(t, tc.yaml))
			assert.Equal(t, tc.wantOK, ok)
			if tc.wantOK {
				assert.Equal(t, tc.wantSchema, schema)
				assert.Equal(t, tc.wantTable, table)
			}
		})
	}
}

func TestTableKeyNoCollision(t *testing.T) {
	a := tableKey{schema: "a.b", table: "c"}
	b := tableKey{schema: "a", table: "b.c"}
	assert.NotEqual(t, a, b)
	m := map[tableKey]tableComments{a: {table: "A"}, b: {table: "B"}}
	assert.Len(t, m, 2)
}

func TestIsAutomaticComment(t *testing.T) {
	assert.True(t, isAutomaticComment(nil))
	cfg := mappingValue(parseTableNode(t, "configuration:\n  comment: null\n"), "configuration")
	assert.True(t, isAutomaticComment(mappingValue(cfg, "comment")))
	cfg = mappingValue(parseTableNode(t, "configuration:\n  comment: \"\"\n"), "configuration")
	assert.False(t, isAutomaticComment(mappingValue(cfg, "comment")))
	cfg = mappingValue(parseTableNode(t, "configuration:\n  comment: hi\n"), "configuration")
	assert.False(t, isAutomaticComment(mappingValue(cfg, "comment")))
}

func TestApplyTableComments_FillsAutomatic(t *testing.T) {
	change, root, err := planOne(t, "table:\n  schema: public\n  name: orders\n", tableComments{
		table:   "all orders",
		columns: map[string]string{"status": "fulfilment status", "id": "pk"},
	})
	require.NoError(t, err)
	require.NotNil(t, change)
	assert.True(t, change.addTable)
	assert.Equal(t, []string{"id", "status"}, change.addCols)

	out := encodeNode(t, root)
	assert.Contains(t, out, "comment: all orders")
	assert.Contains(t, out, "column_config:")
	assert.Contains(t, out, "fulfilment status")
}

func TestApplyTableComments_PreservesExplicitAndSuppressed(t *testing.T) {
	src := "" +
		"table:\n  schema: public\n  name: orders\n" +
		"configuration:\n" +
		"  comment: existing table comment\n" +
		"  custom_name: Orders\n" +
		"  column_config:\n" +
		"    status:\n" +
		"      custom_name: state\n" +
		"      comment: \"\"\n"
	change, root, err := planOne(t, src, tableComments{
		table:   "db table comment",
		columns: map[string]string{"status": "db status comment", "id": "db id comment"},
	})
	require.NoError(t, err)
	require.NotNil(t, change)
	assert.False(t, change.addTable)
	assert.Equal(t, []string{"id"}, change.addCols)

	out := encodeNode(t, root)
	assert.Contains(t, out, "comment: existing table comment")
	assert.Contains(t, out, "custom_name: Orders")
	assert.Contains(t, out, "custom_name: state")
	assert.Contains(t, out, "db id comment")
	assert.NotContains(t, out, "db status comment")
	assert.NotContains(t, out, "db table comment")
}

func TestApplyTableComments_FillsExplicitNull(t *testing.T) {
	change, root, err := planOne(
		t,
		"table:\n  name: orders\nconfiguration:\n  comment: null\n",
		tableComments{table: "filled"},
	)
	require.NoError(t, err)
	require.NotNil(t, change)
	assert.True(t, change.addTable)
	assert.Contains(t, encodeNode(t, root), "comment: filled")
}

func TestApplyTableComments_NoOpWhenNoDBComments(t *testing.T) {
	change, _, err := planOne(t, "table:\n  name: orders\n", tableComments{})
	require.NoError(t, err)
	assert.Nil(t, change)
}

func TestApplyTableComments_Idempotent(t *testing.T) {
	tc := tableComments{table: "c", columns: map[string]string{"id": "idc"}}
	change1, root, err := planOne(t, "table:\n  name: orders\n", tc)
	require.NoError(t, err)
	require.NotNil(t, change1)
	change2, err := planTableNode(
		"default",
		"test.yaml",
		root,
		tableOnly(map[tableKey]tableComments{{schema: "public", table: "orders"}: tc}),
	)
	require.NoError(t, err)
	assert.Nil(t, change2)
}

func TestApplyTableComments_UnicodeAndQuotedIdentifiers(t *testing.T) {
	root := parseTableNode(t, "table:\n  schema: public\n  name: \"wéird\\\"tbl\"\n")
	schema, table, ok := tableIdentifier(root)
	require.True(t, ok)
	assert.Equal(t, `wéird"tbl`, table)
	change, err := planTableNode(
		"default",
		"test.yaml",
		root,
		tableOnly(
			map[tableKey]tableComments{
				{schema: schema, table: table}: {
					table:   "üc",
					columns: map[string]string{`cøl"1`: "colc"},
				},
			},
		),
	)
	require.NoError(t, err)
	require.NotNil(t, change)
	out := encodeNode(t, root)
	assert.Contains(t, out, "üc")
	assert.Contains(t, out, "colc")
}

func TestApplyTableComments_AbortsOnIncludeConfiguration(t *testing.T) {
	root := parseTableNode(t, "table:\n  name: orders\nconfiguration: !include config.yaml\n")
	before := encodeNode(t, root)
	change, err := planTableNode("default", "public_orders.yaml", root,
		tableOnly(map[tableKey]tableComments{{schema: "public", table: "orders"}: {table: "x"}}))
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Contains(t, err.Error(), "cannot safely edit")
	assert.Equal(t, before, encodeNode(t, root))
}

func TestApplyTableComments_AbortsOnAliasConfiguration(t *testing.T) {
	full := "defaults: &cfg\n  custom_name: X\nmytable:\n  table:\n    name: t\n  configuration: *cfg\n"
	var doc yaml.Node
	require.NoError(t, yaml.Unmarshal([]byte(full), &doc))
	tableNode := mappingValue(documentRoot(&doc), "mytable")
	require.NotNil(t, tableNode)
	before := encodeNode(t, documentRoot(&doc))
	change, err := planTableNode("default", "f.yaml", tableNode,
		tableOnly(map[tableKey]tableComments{{schema: "public", table: "t"}: {table: "x"}}))
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Contains(t, err.Error(), "cannot safely edit")
	assert.Equal(t, before, encodeNode(t, documentRoot(&doc)))
}

func TestApplyTableComments_AbortsOnMergeKey(t *testing.T) {
	full := "" +
		"_defaults: &d\n  comment: inherited\n" +
		"mytable:\n  table:\n    name: orders\n  configuration:\n    <<: *d\n    custom_name: Orders\n"
	var doc yaml.Node
	require.NoError(t, yaml.Unmarshal([]byte(full), &doc))
	tableNode := mappingValue(documentRoot(&doc), "mytable")
	before := encodeNode(t, documentRoot(&doc))
	change, err := planTableNode(
		"default",
		"f.yaml",
		tableNode,
		tableOnly(
			map[tableKey]tableComments{{schema: "public", table: "orders"}: {table: "db comment"}},
		),
	)
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Contains(t, err.Error(), "merge key")
	assert.Equal(t, before, encodeNode(t, documentRoot(&doc)))
}

func TestApplyTableComments_NullConfigurationTreatedAsEmpty(t *testing.T) {
	change, root, err := planOne(
		t,
		"table:\n  name: orders\nconfiguration: null\n",
		tableComments{table: "all orders"},
	)
	require.NoError(t, err)
	require.NotNil(t, change)
	assert.True(t, change.addTable)
	assert.Contains(t, encodeNode(t, root), "comment: all orders")
}

//------------------------------------------------------------------------------
// Computed-field comments

// planCF plans a table node that has computed fields, given the function comments.
func planCF(
	t *testing.T,
	src string,
	functions map[tableKey]funcComment,
) (*tableCommentChange, *yaml.Node, error) {
	t.Helper()
	root := parseTableNode(t, src)
	change, err := planTableNode(
		"default",
		"test.yaml",
		root,
		&sourceComments{tables: map[tableKey]tableComments{}, functions: functions},
	)
	return change, root, err
}

const cfTable = "" +
	"table:\n  schema: public\n  name: authors\n" +
	"computed_fields:\n" +
	"  - name: full_name\n" +
	"    definition:\n      function:\n        schema: public\n        name: author_full_name\n"

func TestComputedField_FillsFromFunctionComment(t *testing.T) {
	change, root, err := planCF(t, cfTable, map[tableKey]funcComment{
		{schema: "public", table: "author_full_name"}: {overloads: 1, comment: "the full name"},
	})
	require.NoError(t, err)
	require.NotNil(t, change)
	assert.Equal(t, []string{"full_name"}, change.addComputedFields)
	assert.Contains(t, encodeNode(t, root), "comment: the full name")
}

func TestComputedField_PreservesExplicitComment(t *testing.T) {
	src := cfTable + "    comment: hand written\n"
	change, root, err := planCF(t, src, map[tableKey]funcComment{
		{schema: "public", table: "author_full_name"}: {overloads: 1, comment: "the full name"},
	})
	require.NoError(t, err)
	assert.Nil(t, change)
	assert.Contains(t, encodeNode(t, root), "comment: hand written")
	assert.NotContains(t, encodeNode(t, root), "the full name")
}

func TestComputedField_EmptyFunctionCommentNoOp(t *testing.T) {
	change, _, err := planCF(t, cfTable, map[tableKey]funcComment{
		{schema: "public", table: "author_full_name"}: {overloads: 1, comment: ""},
	})
	require.NoError(t, err)
	assert.Nil(t, change)
}

func TestComputedField_OverloadAmbiguityAborts(t *testing.T) {
	root := parseTableNode(t, cfTable)
	before := encodeNode(t, root)
	change, err := planTableNode("default", "f.yaml", root, &sourceComments{
		tables: map[tableKey]tableComments{},
		functions: map[tableKey]funcComment{
			{schema: "public", table: "author_full_name"}: {overloads: 2},
		},
	})
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Contains(t, err.Error(), "overloads")
	assert.Equal(t, before, encodeNode(t, root), "node must not be mutated on abort")
}

func TestComputedField_Idempotent(t *testing.T) {
	functions := map[tableKey]funcComment{
		{schema: "public", table: "author_full_name"}: {overloads: 1, comment: "c"},
	}
	change1, root, err := planCF(t, cfTable, functions)
	require.NoError(t, err)
	require.NotNil(t, change1)
	change2, err := planTableNode(
		"default",
		"test.yaml",
		root,
		&sourceComments{tables: map[tableKey]tableComments{}, functions: functions},
	)
	require.NoError(t, err)
	assert.Nil(t, change2)
}

//------------------------------------------------------------------------------
// Tracked function comments

const fnEntry = "function:\n  schema: public\n  name: get_orders\nconfiguration:\n  exposed_as: query\npermissions:\n  - role: user\n"

func planFunc(
	t *testing.T,
	src string,
	functions map[tableKey]funcComment,
) (*funcCommentChange, *yaml.Node, error) {
	t.Helper()
	root := parseTableNode(t, src)
	change, err := planFunctionNode("default", "f.yaml", root, functions)
	return change, root, err
}

func fnMap(comment string, overloads int) map[tableKey]funcComment {
	return map[tableKey]funcComment{
		{schema: "public", table: "get_orders"}: {overloads: overloads, comment: comment},
	}
}

func TestFunction_FillsFromComment(t *testing.T) {
	change, root, err := planFunc(t, fnEntry, fnMap("all orders", 1))
	require.NoError(t, err)
	require.NotNil(t, change)
	out := encodeNode(t, root)
	assert.Contains(t, out, "comment: all orders")
	assert.Contains(t, out, "exposed_as: query") // configuration preserved
	assert.Contains(t, out, "role: user")        // permissions preserved
}

func TestFunction_PreservesExplicit(t *testing.T) {
	change, root, err := planFunc(t, fnEntry+"comment: hand written\n", fnMap("all orders", 1))
	require.NoError(t, err)
	assert.Nil(t, change)
	assert.Contains(t, encodeNode(t, root), "hand written")
	assert.NotContains(t, encodeNode(t, root), "all orders")
}

func TestFunction_PreservesSuppressed(t *testing.T) {
	change, _, err := planFunc(t, fnEntry+"comment: \"\"\n", fnMap("all orders", 1))
	require.NoError(t, err)
	assert.Nil(t, change)
}

func TestFunction_FillsNull(t *testing.T) {
	change, root, err := planFunc(t, fnEntry+"comment: null\n", fnMap("c", 1))
	require.NoError(t, err)
	require.NotNil(t, change)
	assert.Contains(t, encodeNode(t, root), "comment: c")
}

func TestFunction_EmptyDBCommentNoOp(t *testing.T) {
	change, _, err := planFunc(t, fnEntry, fnMap("", 1))
	require.NoError(t, err)
	assert.Nil(t, change)
}

func TestFunction_UntrackedNoOp(t *testing.T) {
	change, _, err := planFunc(t, fnEntry, map[tableKey]funcComment{})
	require.NoError(t, err)
	assert.Nil(t, change)
}

func TestFunction_BareStringIdentity(t *testing.T) {
	change, root, err := planFunc(t, "function: get_orders\n", fnMap("c", 1))
	require.NoError(t, err)
	require.NotNil(t, change)
	assert.Contains(t, encodeNode(t, root), "comment: c")
}

func TestFunction_OverloadAmbiguityAborts(t *testing.T) {
	root := parseTableNode(t, fnEntry)
	before := encodeNode(t, root)
	change, err := planFunctionNode("default", "f.yaml", root, fnMap("", 2))
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Contains(t, err.Error(), "overloads")
	assert.Equal(t, before, encodeNode(t, root))
}

func TestFunction_AbortsOnMergeKey(t *testing.T) {
	full := "_d: &d\n  comment: inherited\nitem:\n  function:\n    schema: public\n    name: get_orders\n  <<: *d\n"
	var doc yaml.Node
	require.NoError(t, yaml.Unmarshal([]byte(full), &doc))
	item := mappingValue(documentRoot(&doc), "item")
	before := encodeNode(t, documentRoot(&doc))
	change, err := planFunctionNode("default", "f.yaml", item, fnMap("c", 1))
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Equal(t, before, encodeNode(t, documentRoot(&doc)))
}

func TestFunction_Idempotent(t *testing.T) {
	functions := fnMap("c", 1)
	change1, root, err := planFunc(t, fnEntry, functions)
	require.NoError(t, err)
	require.NotNil(t, change1)
	change2, err := planFunctionNode("default", "f.yaml", root, functions)
	require.NoError(t, err)
	assert.Nil(t, change2)
}

func TestPlanFunctionFileEdit_PerFunctionFile(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "public_get_orders.yaml")
	require.NoError(t, os.WriteFile(file, []byte(fnEntry), 0o644))
	edit, err := planFunctionFileEdit("default", file, fnMap("c", 1))
	require.NoError(t, err)
	require.NotNil(t, edit)
	assert.Len(t, edit.funcChanges, 1)
}

func TestPlanFunctionFileEdit_IndexFileSkipped(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "functions.yaml")
	require.NoError(t, os.WriteFile(file, []byte("- \"!include public_get_orders.yaml\"\n"), 0o644))
	edit, err := planFunctionFileEdit("default", file, fnMap("c", 1))
	require.NoError(t, err)
	assert.Nil(t, edit)
}

func TestPlanFunctionFileEdit_AliasEntryAborts(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "functions.yaml")
	content := "- &b\n    function:\n      schema: public\n      name: get_orders\n- *b\n"
	require.NoError(t, os.WriteFile(file, []byte(content), 0o644))
	_, err := planFunctionFileEdit("default", file, fnMap("c", 1))
	require.Error(t, err)
}

func TestFunction_AbortsOnIncludeFunctionRef(t *testing.T) {
	root := parseTableNode(t, "function: !include fn.yaml\n")
	before := encodeNode(t, root)
	change, err := planFunctionNode("default", "f.yaml", root, fnMap("c", 1))
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Equal(t, before, encodeNode(t, root))
}

//------------------------------------------------------------------------------
// Conservative layout validation for computed fields

func planCFraw(t *testing.T, src string) (*tableCommentChange, *yaml.Node, error) {
	t.Helper()
	root := parseTableNode(t, src)
	functions := map[tableKey]funcComment{
		{schema: "public", table: "author_full_name"}: {overloads: 1, comment: "c"},
	}
	change, err := planTableNode(
		"default",
		"f.yaml",
		root,
		&sourceComments{tables: map[tableKey]tableComments{}, functions: functions},
	)
	return change, root, err
}

func TestComputedField_UnsupportedIncludeLayoutAborts(t *testing.T) {
	change, root, err := planCFraw(
		t,
		"table:\n  schema: public\n  name: authors\ncomputed_fields: !include cfs.yaml\n",
	)
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Contains(t, err.Error(), "computed_fields")
	assert.NotContains(t, encodeNode(t, root), "comment:")
}

func TestComputedField_AliasEntryAborts(t *testing.T) {
	full := "_d: &d\n  name: full_name\n  definition:\n    function:\n      schema: public\n      name: author_full_name\nt:\n  table:\n    schema: public\n    name: authors\n  computed_fields:\n    - *d\n"
	var doc yaml.Node
	require.NoError(t, yaml.Unmarshal([]byte(full), &doc))
	tnode := mappingValue(documentRoot(&doc), "t")
	before := encodeNode(t, documentRoot(&doc))
	functions := map[tableKey]funcComment{
		{schema: "public", table: "author_full_name"}: {overloads: 1, comment: "c"},
	}
	change, err := planTableNode(
		"default",
		"f.yaml",
		tnode,
		&sourceComments{tables: map[tableKey]tableComments{}, functions: functions},
	)
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Equal(t, before, encodeNode(t, documentRoot(&doc)))
}

func TestComputedField_IncludeFunctionRefAborts(t *testing.T) {
	change, _, err := planCFraw(
		t,
		"table:\n  schema: public\n  name: authors\ncomputed_fields:\n  - name: full_name\n    definition:\n      function: !include fn.yaml\n",
	)
	require.Error(t, err)
	assert.Nil(t, change)
	assert.Contains(t, err.Error(), "function")
}

func TestComputedField_NullComputedFieldsNoOp(t *testing.T) {
	change, _, err := planCFraw(
		t,
		"table:\n  schema: public\n  name: authors\ncomputed_fields: null\n",
	)
	require.NoError(t, err)
	assert.Nil(t, change)
}

func TestRun_UnsafeFunctionLayoutAbortsBeforeWrite(t *testing.T) {
	metadataDir, tableFile, functionFile := writeProject(t, "postgres")
	// Make the functions file an unsafe layout (alias entry).
	require.NoError(
		t,
		os.WriteFile(
			functionFile,
			[]byte("- &b\n    function:\n      schema: public\n      name: get_orders\n- *b\n"),
			0o644,
		),
	)
	beforeTable, err := os.ReadFile(tableFile)
	require.NoError(t, err)

	var requests []recordedRequest
	var mu sync.Mutex
	v2 := newTestV2Query(t, &requests, &mu)

	err = runMigratePostgresComments(discardLogger(), metadataDir, v2, "", false)
	require.Error(t, err)
	afterTable, err2 := os.ReadFile(tableFile)
	require.NoError(t, err2)
	assert.Equal(
		t,
		string(beforeTable),
		string(afterTable),
		"no file should be written when a later file has an unsafe layout",
	)
}

//------------------------------------------------------------------------------
// run_sql result handling

func TestFetchSourceComments(t *testing.T) {
	runSQL := func(sql string) (*hasura.PGRunSQLOutput, error) {
		switch {
		case strings.Contains(sql, "attname"):
			return &hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: [][]string{
				{"schema", "table", "column", "comment"},
				{"public", "orders", "status", "the status"},
			}}, nil
		case strings.Contains(sql, "pg_proc"):
			return &hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: [][]string{
				{"schema", "name", "comment"},
				{"public", "f1", "f1 comment"},
				{"public", "f2", ""},
				{"public", "f2", ""}, // two overloads of f2
			}}, nil
		default:
			return &hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: [][]string{
				{"schema", "table", "comment"},
				{"public", "orders", "all orders"},
			}}, nil
		}
	}
	got, err := fetchSourceComments(hasura.SourceKindPG, runSQL)
	require.NoError(t, err)
	assert.Equal(t, "all orders", got.tables[tableKey{schema: "public", table: "orders"}].table)
	assert.Equal(
		t,
		"the status",
		got.tables[tableKey{schema: "public", table: "orders"}].columns["status"],
	)
	assert.Equal(
		t,
		funcComment{overloads: 1, comment: "f1 comment"},
		got.functions[tableKey{schema: "public", table: "f1"}],
	)
	assert.Equal(t, 2, got.functions[tableKey{schema: "public", table: "f2"}].overloads)
}

func TestFetchSourceComments_CockroachSkipsColumnsAndFunctions(t *testing.T) {
	var queried []string
	runSQL := func(sql string) (*hasura.PGRunSQLOutput, error) {
		switch {
		case strings.Contains(sql, "attname"):
			queried = append(queried, "column")
		case strings.Contains(sql, "pg_proc"):
			queried = append(queried, "function")
		default:
			queried = append(queried, "table")
		}
		return &hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: [][]string{
			{"schema", "table", "comment"},
			{"public", "orders", "all orders"},
		}}, nil
	}
	got, err := fetchSourceComments(hasura.SourceKindCockroach, runSQL)
	require.NoError(t, err)
	assert.Equal(t, []string{"table"}, queried, "cockroach must only run the table comment query")
	assert.Empty(t, got.functions)
}

func TestDataRowsValidation(t *testing.T) {
	_, err := dataRows(nil, 3)
	require.Error(t, err)
	_, err = dataRows(&hasura.PGRunSQLOutput{ResultType: hasura.CommandOK}, 3)
	require.Error(t, err)
	_, err = dataRows(&hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: nil}, 3)
	require.Error(t, err)
	_, err = dataRows(
		&hasura.PGRunSQLOutput{
			ResultType: hasura.TuplesOK,
			Result:     [][]string{{"a", "b", "c"}, {"x", "y"}},
		},
		3,
	)
	require.Error(t, err)
	rows, err := dataRows(
		&hasura.PGRunSQLOutput{
			ResultType: hasura.TuplesOK,
			Result:     [][]string{{"a", "b", "c"}, {"x", "y", "z"}},
		},
		3,
	)
	require.NoError(t, err)
	assert.Len(t, rows, 1)
}

func TestFetchSourceComments_MalformedResultErrors(t *testing.T) {
	runSQL := func(sql string) (*hasura.PGRunSQLOutput, error) {
		return &hasura.PGRunSQLOutput{ResultType: hasura.CommandOK}, nil
	}
	_, err := fetchSourceComments(hasura.SourceKindPG, runSQL)
	require.Error(t, err)
}

func TestCatalogSQLIsInjectionSafe(t *testing.T) {
	for _, sql := range []string{tableCommentsSQL(), columnCommentsSQL(), functionCommentsSQL()} {
		assert.NotContains(t, sql, "%s")
		assert.NotContains(t, sql, "%q")
	}
	assert.Contains(t, functionCommentsSQL(), "pg_proc")
	assert.Contains(t, functionCommentsSQL(), "obj_description")
	assert.Contains(t, columnCommentsSQL(), "attname")
	assert.NotContains(t, tableCommentsSQL(), "attname")
}

//------------------------------------------------------------------------------
// File planning / writing

func TestPlanFileEdit_SequenceOfInlineMappings(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "tables.yaml")
	require.NoError(
		t,
		os.WriteFile(file, []byte("- table:\n    name: a\n- table:\n    name: b\n"), 0o644),
	)
	edit, err := planFileEdit("default", file, tableOnly(map[tableKey]tableComments{
		{schema: "public", table: "a"}: {table: "ca"},
		{schema: "public", table: "b"}: {table: "cb"},
	}))
	require.NoError(t, err)
	require.NotNil(t, edit)
	assert.Len(t, edit.changes, 2)
}

func TestPlanFileEdit_IndexFileSkipped(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "tables.yaml")
	require.NoError(t, os.WriteFile(file, []byte("- \"!include public_a.yaml\"\n"), 0o644))
	edit, err := planFileEdit(
		"default",
		file,
		tableOnly(map[tableKey]tableComments{{schema: "public", table: "a"}: {table: "ca"}}),
	)
	require.NoError(t, err)
	assert.Nil(t, edit)
}

func TestPlanFileEdit_MalformedTableErrors(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "public_orders.yaml")
	require.NoError(t, os.WriteFile(file, []byte("table:\n  schema: public\n"), 0o644))
	_, err := planFileEdit(
		"default",
		file,
		tableOnly(map[tableKey]tableComments{{schema: "public", table: "orders"}: {table: "c"}}),
	)
	require.Error(t, err)
}

func TestWriteFileEdits_AtomicPreservesPerms(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "public_orders.yaml")
	require.NoError(t, os.WriteFile(file, []byte("table:\n  name: orders\n"), 0o600))
	edit, err := planFileEdit(
		"default",
		file,
		tableOnly(
			map[tableKey]tableComments{{schema: "public", table: "orders"}: {table: "all orders"}},
		),
	)
	require.NoError(t, err)
	require.NotNil(t, edit)
	require.NoError(t, writeFileEdits([]fileEdit{*edit}))

	info, err := os.Stat(file)
	require.NoError(t, err)
	assert.Equal(t, os.FileMode(0o600), info.Mode().Perm())
	data, err := os.ReadFile(file)
	require.NoError(t, err)
	assert.Contains(t, string(data), "comment: all orders")
}

//------------------------------------------------------------------------------
// Command-level tests with an httptest run_sql server

type recordedRequest struct {
	Type string `json:"type"`
	Args struct {
		SQL                      string `json:"sql"`
		Source                   string `json:"source"`
		ReadOnly                 bool   `json:"read_only"`
		CheckMetadataConsistency *bool  `json:"check_metadata_consistency"`
	} `json:"args"`
}

func newTestV2Query(t *testing.T, requests *[]recordedRequest, mu *sync.Mutex) hasura.V2Query {
	t.Helper()
	handler := func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		var req recordedRequest
		_ = json.Unmarshal(body, &req)
		mu.Lock()
		*requests = append(*requests, req)
		mu.Unlock()

		var out hasura.PGRunSQLOutput
		switch {
		case strings.Contains(req.Args.SQL, "attname"):
			out = hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: [][]string{
				{"schema", "table", "column", "comment"},
				{"public", "orders", "status", "fulfilment status"},
			}}
		case strings.Contains(req.Args.SQL, "pg_proc"):
			out = hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: [][]string{
				{"schema", "name", "comment"},
				{"public", "author_full_name", "the full name"},
				{"public", "get_orders", "returns all orders"},
			}}
		default:
			out = hasura.PGRunSQLOutput{ResultType: hasura.TuplesOK, Result: [][]string{
				{"schema", "table", "comment"},
				{"public", "orders", "all orders"},
			}}
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(out)
	}
	srv := httptest.NewServer(http.HandlerFunc(handler))
	t.Cleanup(srv.Close)
	c, err := httpc.New(nil, srv.URL+"/", nil)
	require.NoError(t, err)
	return v2query.New(c, "v2/query")
}

func discardLogger() *logrus.Logger {
	l := logrus.New()
	l.SetOutput(io.Discard)
	return l
}

// writeProject creates a minimal config v3 layout with one source, one tracked
// table (with a computed field) and one tracked function.
func writeProject(t *testing.T, kind string) (metadataDir, tableFile, functionFile string) {
	t.Helper()
	root := t.TempDir()
	metadataDir = filepath.Join(root, "metadata")
	tablesDir := filepath.Join(metadataDir, "databases", "default", "tables")
	functionsDir := filepath.Join(metadataDir, "databases", "default", "functions")
	require.NoError(t, os.MkdirAll(tablesDir, 0o755))
	require.NoError(t, os.MkdirAll(functionsDir, 0o755))

	databases := "- name: default\n  kind: " + kind + "\n  tables: !include default/tables/tables.yaml\n  functions: !include default/functions/functions.yaml\n"
	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(metadataDir, "databases", "databases.yaml"),
			[]byte(databases),
			0o644,
		),
	)

	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(tablesDir, "tables.yaml"),
			[]byte("- \"!include public_orders.yaml\"\n"),
			0o644,
		),
	)
	tableFile = filepath.Join(tablesDir, "public_orders.yaml")
	tableContent := "" +
		"table:\n  schema: public\n  name: orders\n" +
		"computed_fields:\n  - name: full_name\n    definition:\n      function:\n        schema: public\n        name: author_full_name\n"
	require.NoError(t, os.WriteFile(tableFile, []byte(tableContent), 0o644))

	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(functionsDir, "functions.yaml"),
			[]byte("- \"!include public_get_orders.yaml\"\n"),
			0o644,
		),
	)
	functionFile = filepath.Join(functionsDir, "public_get_orders.yaml")
	functionContent := "" +
		"function:\n  schema: public\n  name: get_orders\n" +
		"configuration:\n  exposed_as: query\n"
	require.NoError(t, os.WriteFile(functionFile, []byte(functionContent), 0o644))
	return metadataDir, tableFile, functionFile
}

func TestRun_WritesCommentsViaRunSQL(t *testing.T) {
	metadataDir, tableFile, functionFile := writeProject(t, "postgres")
	var requests []recordedRequest
	var mu sync.Mutex
	v2 := newTestV2Query(t, &requests, &mu)

	require.NoError(t, runMigratePostgresComments(discardLogger(), metadataDir, v2, "", false))

	data, err := os.ReadFile(tableFile)
	require.NoError(t, err)
	assert.Contains(t, string(data), "comment: all orders")    // table
	assert.Contains(t, string(data), "fulfilment status")      // column
	assert.Contains(t, string(data), "comment: the full name") // computed field

	fdata, err := os.ReadFile(functionFile)
	require.NoError(t, err)
	assert.Contains(t, string(fdata), "comment: returns all orders") // tracked function
	assert.Contains(t, string(fdata), "exposed_as: query")           // function config preserved

	require.NotEmpty(t, requests)
	sawFunction := false
	for _, req := range requests {
		assert.Equal(t, "run_sql", req.Type)
		assert.True(t, req.Args.ReadOnly)
		assert.Equal(t, "default", req.Args.Source)
		require.NotNil(t, req.Args.CheckMetadataConsistency)
		assert.False(t, *req.Args.CheckMetadataConsistency)
		if strings.Contains(req.Args.SQL, "pg_proc") {
			sawFunction = true
		}
	}
	assert.True(t, sawFunction, "the function comment query should have been issued")
}

func TestRun_DryRunWritesNothing(t *testing.T) {
	metadataDir, tableFile, _ := writeProject(t, "postgres")
	before, err := os.ReadFile(tableFile)
	require.NoError(t, err)
	var requests []recordedRequest
	var mu sync.Mutex
	v2 := newTestV2Query(t, &requests, &mu)

	require.NoError(t, runMigratePostgresComments(discardLogger(), metadataDir, v2, "", true))

	after, err := os.ReadFile(tableFile)
	require.NoError(t, err)
	assert.Equal(t, string(before), string(after))
}

func TestRun_UnsupportedSourceRejected(t *testing.T) {
	root := t.TempDir()
	metadataDir := filepath.Join(root, "metadata")
	require.NoError(t, os.MkdirAll(filepath.Join(metadataDir, "databases"), 0o755))
	databases := "- name: bq\n  kind: bigquery\n  tables: []\n"
	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(metadataDir, "databases", "databases.yaml"),
			[]byte(databases),
			0o644,
		),
	)

	var requests []recordedRequest
	var mu sync.Mutex
	v2 := newTestV2Query(t, &requests, &mu)

	err := runMigratePostgresComments(discardLogger(), metadataDir, v2, "bq", false)
	require.Error(t, err)
	assert.Contains(t, err.Error(), "only supports Postgres-family")
	assert.Empty(t, requests)
}

func TestRun_AbortsBeforeWriteOnMalformedFile(t *testing.T) {
	metadataDir, tableFile, _ := writeProject(t, "postgres")
	require.NoError(
		t,
		os.WriteFile(tableFile, []byte("table:\n  schema: public\n"), 0o644),
	) // no name
	before, err := os.ReadFile(tableFile)
	require.NoError(t, err)
	var requests []recordedRequest
	var mu sync.Mutex
	v2 := newTestV2Query(t, &requests, &mu)

	err = runMigratePostgresComments(discardLogger(), metadataDir, v2, "", false)
	require.Error(t, err)
	after, err2 := os.ReadFile(tableFile)
	require.NoError(t, err2)
	assert.Equal(t, string(before), string(after))
}

func TestRun_DuplicateFileAcrossSourcesRejected(t *testing.T) {
	root := t.TempDir()
	metadataDir := filepath.Join(root, "metadata")
	tablesDir := filepath.Join(metadataDir, "databases", "shared", "tables")
	require.NoError(t, os.MkdirAll(tablesDir, 0o755))
	databases := "" +
		"- name: s1\n  kind: postgres\n  tables: !include shared/tables/tables.yaml\n" +
		"- name: s2\n  kind: postgres\n  tables: !include shared/tables/tables.yaml\n"
	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(metadataDir, "databases", "databases.yaml"),
			[]byte(databases),
			0o644,
		),
	)
	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(tablesDir, "tables.yaml"),
			[]byte("- \"!include public_orders.yaml\"\n"),
			0o644,
		),
	)
	tableFile := filepath.Join(tablesDir, "public_orders.yaml")
	require.NoError(
		t,
		os.WriteFile(tableFile, []byte("table:\n  schema: public\n  name: orders\n"), 0o644),
	)
	before, err := os.ReadFile(tableFile)
	require.NoError(t, err)

	var requests []recordedRequest
	var mu sync.Mutex
	v2 := newTestV2Query(t, &requests, &mu)

	err = runMigratePostgresComments(discardLogger(), metadataDir, v2, "", false)
	require.Error(t, err)
	assert.Contains(t, err.Error(), "referenced by more than one source")
	after, err2 := os.ReadFile(tableFile)
	require.NoError(t, err2)
	assert.Equal(t, string(before), string(after))
}

func TestCommandRegistration(t *testing.T) {
	codemod := newMetadataCodemodCmd(nil, nil)
	assert.Equal(t, "codemod", codemod.Name())
	var names []string
	for _, c := range codemod.Commands() {
		names = append(names, c.Name())
	}
	assert.Contains(t, names, "migrate-postgres-comments")

	migrate := newMetadataCodemodMigratePostgresCommentsCmd(nil)
	assert.NotNil(t, migrate.Flags().Lookup("source"))
	assert.NotNil(t, migrate.Flags().Lookup("dry-run"))
}
