package commands

import (
	"bytes"
	stderrors "errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	yaml "go.yaml.in/yaml/v3"
)

const includeTag = "!include"

// Postgres-family source kinds whose table/column COMMENTs this codemod can read
// from the database catalogs.
var pgFamilySourceKinds = map[hasura.SourceKind]bool{
	hasura.SourceKindPG:        true,
	hasura.SourceKindCitus:     true,
	hasura.SourceKindCockroach: true,
}

func newMetadataCodemodMigratePostgresCommentsCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataCodemodMigratePostgresCommentsOptions{EC: ec}

	cmd := &cobra.Command{
		Use:   "migrate-postgres-comments",
		Short: "Copy PostgreSQL table, column, function and computed-field COMMENTs into local Hasura Metadata",
		Long: `Read table, column and function COMMENTs from the database catalogs of
your Postgres-family sources (postgres, citus, cockroach) and copy them into the
local project Metadata, so they are preserved as GraphQL descriptions after Hasura
stops importing database COMMENTs automatically (now deprecated).

What is migrated and where:
- table COMMENT            -> configuration.comment
- column COMMENT           -> configuration.column_config.COLUMN_NAME.comment
- computed-field function COMMENT -> the computed field's 'comment'
- tracked-function COMMENT -> the function's top-level 'comment'

Computed-field comments restore prior behaviour: Hasura previously used the backing
function's database COMMENT as the computed field's description. Tracked-function
comments are also migrated, as requested; note that Hasura does not render a tracked
function's database COMMENT today (the function's query-root description comes from
the metadata 'comment' only), so this moves the database text into metadata as a new
query-root description rather than restoring one. Mutation and aggregate root
defaults are unchanged.

Run it, review the diff, then 'hasura metadata apply' to push the changes to the
server.

Only tracked tables, their computed fields, and tracked functions are touched,
matched by their actual database schema/table/column/function names. Existing
Metadata is preserved: comments that are already set (including an empty string,
which suppresses the description) are never overwritten, and custom names / root
fields / column, computed-field and function configuration/permissions/definitions
are left intact. The command only writes to local Metadata files; it never applies
to the server.

For CockroachDB only table comments are migrated (column and function descriptions
were never imported for Cockroach).

This command supports config v3 projects whose tables and functions are stored as
separate !include files. It refuses to run (before writing anything) on metadata it
cannot edit safely, so that existing metadata is never clobbered: a table, computed
field or function whose node is itself an !include, a YAML alias, or uses a YAML
merge key; a metadata file shared by more than one source; or a computed field or
tracked function whose database function has multiple overloads (the comment cannot
be attributed unambiguously, since function metadata does not encode the argument
signature). A 'configuration: null' is treated as empty.`,
		Example: `  # Preview the changes without writing anything:
  hasura metadata codemod migrate-postgres-comments --dry-run

  # Migrate comments for all Postgres-family sources into local metadata:
  hasura metadata codemod migrate-postgres-comments

  # Migrate comments for a single source:
  hasura metadata codemod migrate-postgres-comments --source default

  # Afterwards, review the diff and apply to the server:
  hasura metadata apply`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if err := opts.run(); err != nil {
				return errors.E(op, err)
			}

			return nil
		},
	}

	f := cmd.Flags()
	f.StringVar(
		&opts.source,
		"source",
		"",
		"the Postgres-family source to migrate comments for (default: all Postgres-family sources)",
	)
	f.BoolVar(
		&opts.dryRun,
		"dry-run",
		false,
		"print the proposed metadata changes without writing any files",
	)

	return cmd
}

type metadataCodemodMigratePostgresCommentsOptions struct {
	EC     *cli.ExecutionContext
	source string
	dryRun bool
}

func (o *metadataCodemodMigratePostgresCommentsOptions) run() error {
	var op errors.Op = "commands.metadataCodemodMigratePostgresCommentsOptions.run"
	if err := runMigratePostgresComments(
		o.EC.Logger,
		o.EC.MetadataDir,
		o.EC.APIClient.V2Query,
		o.source,
		o.dryRun,
	); err != nil {
		return errors.E(op, err)
	}

	return nil
}

// tableKey uniquely identifies a table. Using a struct (rather than a joined
// string) avoids identifier collisions such as ("a.b","c") vs ("a","b.c").
type tableKey struct {
	schema string
	table  string
}

func (k tableKey) String() string { return k.schema + "." + k.table }

// tableComments holds the database COMMENTs for a single table.
type tableComments struct {
	table   string            // table comment ("" when none)
	columns map[string]string // column name -> comment
}

// funcComment holds the database COMMENT for a function, together with how many
// overloads exist for its (schema, name). Postgres function metadata does not
// encode the argument signature, so a comment can only be attributed
// unambiguously when there is exactly one overload.
type funcComment struct {
	overloads int
	comment   string // the single overload's comment; meaningful only when overloads == 1
}

// sourceComments is everything read from one source's database catalogs.
type sourceComments struct {
	tables    map[tableKey]tableComments
	functions map[tableKey]funcComment
}

// tableCommentChange records, for reporting, what was added to a single table.
type tableCommentChange struct {
	source            string
	schema            string
	table             string
	addTable          bool
	addCols           []string
	addComputedFields []string
}

// funcCommentChange records, for reporting, a tracked function whose top-level
// 'comment' was filled from its database COMMENT.
type funcCommentChange struct {
	source string
	schema string
	name   string
}

// fileEdit is a single metadata file whose in-memory document has been modified
// and is pending write.
type fileEdit struct {
	path        string
	doc         *yaml.Node
	changes     []tableCommentChange
	funcChanges []funcCommentChange
}

// runMigratePostgresComments is the testable core of the command: it reads
// comments from the database catalogs (via the given V2Query client) and edits
// local metadata, gathering every change across all target sources before writing
// anything. When dryRun is set nothing is written.
func runMigratePostgresComments(
	logger *logrus.Logger,
	metadataDir string,
	v2 hasura.V2Query,
	source string,
	dryRun bool,
) error {
	var op errors.Op = "commands.runMigratePostgresComments"

	databasesFile := filepath.Join(metadataDir, "databases", "databases.yaml")
	if _, err := os.Stat(databasesFile); err != nil {
		return errors.E(
			op,
			fmt.Errorf(
				"could not find %s; this command requires a config v3 project with metadata exported locally: %w",
				databasesFile,
				err,
			),
		)
	}

	sources, err := loadSources(databasesFile)
	if err != nil {
		return errors.E(op, err)
	}

	targets, err := resolveTargets(sources, source, databasesFile)
	if err != nil {
		return errors.E(op, err)
	}

	if len(targets) == 0 {
		logger.Info("No Postgres-family sources found in local metadata; nothing to do.")

		return nil
	}

	// Gather every file edit across all sources *before* writing anything, so a
	// parse/type/transport error aborts the whole migration cleanly.
	var edits []fileEdit

	for _, src := range targets {
		comments, err := fetchSourceComments(src.kind, makeRunSQL(v2, src))
		if err != nil {
			return errors.E(
				op,
				fmt.Errorf("failed to read comments for source %q: %w", src.name, err),
			)
		}

		srcEdits, err := planSourceEdits(src, comments)
		if err != nil {
			return errors.E(
				op,
				fmt.Errorf("failed to plan changes for source %q: %w", src.name, err),
			)
		}

		edits = append(edits, srcEdits...)
	}

	if len(edits) == 0 {
		logger.Info("No comments to migrate; local metadata is already up to date.")

		return nil
	}

	// If two sources resolve to the same physical metadata file, writing would
	// silently discard one source's edits. Refuse before writing anything.
	if dup := duplicateEditPath(edits); dup != "" {
		return errors.E(
			op,
			fmt.Errorf(
				"metadata file %s is referenced by more than one source; refusing to edit it to avoid conflicting writes",
				dup,
			),
		)
	}

	printChanges(logger, edits)

	if dryRun {
		logger.Info(
			"Dry run: no files were written. Re-run without --dry-run to apply these changes to local metadata.",
		)

		return nil
	}

	if err := writeFileEdits(edits); err != nil {
		return errors.E(op, err)
	}

	logger.Infof(
		"Migrated comments into local metadata. Review the changes and run 'hasura metadata apply' to update the server.",
	)

	return nil
}

// duplicateEditPath returns the first (sorted) canonical path that appears in
// more than one fileEdit, or "" if there are none.
func duplicateEditPath(edits []fileEdit) string {
	counts := map[string]int{}
	display := map[string]string{}

	for _, e := range edits {
		canon := e.path
		if abs, err := filepath.Abs(e.path); err == nil {
			canon = filepath.Clean(abs)
		}

		counts[canon]++
		display[canon] = e.path
	}

	var dups []string

	for canon, n := range counts {
		if n > 1 {
			dups = append(dups, display[canon])
		}
	}

	if len(dups) == 0 {
		return ""
	}

	sort.Strings(dups)

	return dups[0]
}

func printChanges(logger *logrus.Logger, edits []fileEdit) {
	tables, cols, computed, funcs, tableCount := 0, 0, 0, 0, 0

	for _, e := range edits {
		for _, c := range e.changes {
			tableCount++

			logger.Infof("source %q, table %s.%s (%s):", c.source, c.schema, c.table, e.path)

			if c.addTable {
				tables++

				logger.Infof("  + configuration.comment")
			}

			for _, col := range c.addCols {
				cols++

				logger.Infof("  + configuration.column_config.%s.comment", col)
			}

			for _, cf := range c.addComputedFields {
				computed++

				logger.Infof("  + computed_fields[%s].comment", cf)
			}
		}

		for _, fc := range e.funcChanges {
			funcs++

			logger.Infof("source %q, function %s.%s (%s):", fc.source, fc.schema, fc.name, e.path)
			logger.Infof("  + comment")
		}
	}

	logger.Infof(
		"Total: %d table, %d column, %d computed-field and %d tracked-function comment(s).",
		tables,
		cols,
		computed,
		funcs,
	)
}

//------------------------------------------------------------------------------
// Reading sources from local metadata

type metadataSource struct {
	name          string
	kind          hasura.SourceKind
	tablesNode    *yaml.Node
	functionsNode *yaml.Node
	databasesDir  string
}

func loadSources(databasesFile string) ([]metadataSource, error) {
	var op errors.Op = "commands.loadSources"

	contents, err := os.ReadFile(databasesFile)
	if err != nil {
		return nil, errors.E(op, err)
	}

	var doc yaml.Node
	if err := yaml.Unmarshal(contents, &doc); err != nil {
		return nil, errors.E(op, fmt.Errorf("parsing %s: %w", databasesFile, err))
	}

	seq := documentRoot(&doc)
	if seq == nil || seq.Kind != yaml.SequenceNode {
		return nil, errors.E(
			op,
			fmt.Errorf("expected %s to contain a list of sources", databasesFile),
		)
	}

	var sources []metadataSource

	for _, sourceNode := range seq.Content {
		if sourceNode.Kind != yaml.MappingNode {
			continue
		}

		nameNode := mappingValue(sourceNode, "name")

		kindNode := mappingValue(sourceNode, "kind")
		if nameNode == nil || kindNode == nil {
			continue
		}

		sources = append(sources, metadataSource{
			name:          nameNode.Value,
			kind:          hasura.SourceKind(kindNode.Value),
			tablesNode:    mappingValue(sourceNode, "tables"),
			functionsNode: mappingValue(sourceNode, "functions"),
			databasesDir:  filepath.Dir(databasesFile),
		})
	}

	return sources, nil
}

func resolveTargets(
	sources []metadataSource,
	source, databasesFile string,
) ([]metadataSource, error) {
	var op errors.Op = "commands.resolveTargets"

	if source != "" {
		for _, s := range sources {
			if s.name == source {
				if !pgFamilySourceKinds[s.kind] {
					return nil, errors.E(
						op,
						fmt.Errorf(
							"source %q has kind %q; migrate-postgres-comments only supports Postgres-family sources (postgres, citus, cockroach)",
							s.name,
							s.kind,
						),
					)
				}

				return []metadataSource{s}, nil
			}
		}

		return nil, errors.E(op, fmt.Errorf("source %q was not found in %s", source, databasesFile))
	}

	var targets []metadataSource

	for _, s := range sources {
		if pgFamilySourceKinds[s.kind] {
			targets = append(targets, s)
		}
	}

	return targets, nil
}

//------------------------------------------------------------------------------
// Planning changes

func planSourceEdits(src metadataSource, comments *sourceComments) ([]fileEdit, error) {
	var op errors.Op = "commands.planSourceEdits"

	if len(comments.tables) == 0 && len(comments.functions) == 0 {
		return nil, nil
	}

	var edits []fileEdit

	seen := map[string]bool{}

	// Table (and computed-field) edits, from the tables !include tree.
	if len(comments.tables) > 0 || len(comments.functions) > 0 {
		tableFiles, err := includeFiles(src, src.tablesNode, "tables")
		if err != nil {
			return nil, errors.E(op, err)
		}

		for _, file := range tableFiles {
			if seen[file] {
				continue
			}

			seen[file] = true

			edit, err := planFileEdit(src.name, file, comments)
			if err != nil {
				return nil, errors.E(op, err)
			}

			if edit != nil {
				edits = append(edits, *edit)
			}
		}
	}

	// Tracked-function edits, from the functions !include tree.
	if len(comments.functions) > 0 {
		functionFiles, err := includeFiles(src, src.functionsNode, "functions")
		if err != nil {
			return nil, errors.E(op, err)
		}

		for _, file := range functionFiles {
			if seen[file] {
				continue
			}

			seen[file] = true

			edit, err := planFunctionFileEdit(src.name, file, comments.functions)
			if err != nil {
				return nil, errors.E(op, err)
			}

			if edit != nil {
				edits = append(edits, *edit)
			}
		}
	}

	return edits, nil
}

// includeFiles resolves the per-entry !include files under a source's "tables" or
// "functions" node. It refuses inline (non-!include) sequences, which would require
// rewriting databases.yaml (and its source connection config).
func includeFiles(src metadataSource, node *yaml.Node, what string) ([]string, error) {
	var op errors.Op = "commands.includeFiles"

	if node == nil {
		return nil, nil
	}

	files, err := metadatautil.GetIncludeTagFiles(node, src.databasesDir)
	if err != nil {
		return nil, errors.E(op, err)
	}

	if len(files) == 0 && node.Kind == yaml.SequenceNode && len(node.Content) > 0 {
		return nil, errors.E(
			op,
			fmt.Errorf(
				"source %q stores its %s inline in databases.yaml; this codemod only edits %s stored as separate !include files",
				src.name,
				what,
				what,
			),
		)
	}

	return files, nil
}

// planFileEdit reads one metadata file referenced from the tables !include tree
// and stages comment additions for any table entries it contains. It returns nil
// when nothing changes, and an error (before any write) when the file cannot be
// edited safely.
func planFileEdit(source, file string, comments *sourceComments) (*fileEdit, error) {
	var op errors.Op = "commands.planFileEdit"

	contents, err := os.ReadFile(file)
	if err != nil {
		return nil, errors.E(op, err)
	}

	var doc yaml.Node
	if err := yaml.Unmarshal(contents, &doc); err != nil {
		return nil, errors.E(op, fmt.Errorf("parsing %s: %w", file, err))
	}

	root := documentRoot(&doc)
	if root == nil {
		return nil, nil
	}

	// Collect the table-entry mapping nodes contained in this file. Index files
	// (a sequence of !include scalars) and non-table files contribute none; their
	// real tables are processed via their own files in the include tree.
	var tableNodes []*yaml.Node

	switch root.Kind {
	case yaml.MappingNode:
		if mappingValue(root, "table") != nil {
			tableNodes = append(tableNodes, root)
		}
	case yaml.SequenceNode:
		for _, entry := range root.Content {
			if entry.Kind == yaml.MappingNode && mappingValue(entry, "table") != nil {
				tableNodes = append(tableNodes, entry)
			}
		}
	}

	var changes []tableCommentChange

	for _, tableNode := range tableNodes {
		change, err := planTableNode(source, file, tableNode, comments)
		if err != nil {
			return nil, errors.E(op, err)
		}

		if change != nil {
			changes = append(changes, *change)
		}
	}

	if len(changes) == 0 {
		return nil, nil
	}

	return &fileEdit{path: file, doc: &doc, changes: changes}, nil
}

func planTableNode(
	source, file string,
	tableRoot *yaml.Node,
	comments *sourceComments,
) (*tableCommentChange, error) {
	var op errors.Op = "commands.planTableNode"

	schema, table, ok := tableIdentifier(tableRoot)
	if !ok {
		return nil, errors.E(
			op,
			fmt.Errorf("%s: table entry has no usable 'table' identifier", file),
		)
	}

	change := &tableCommentChange{source: source, schema: schema, table: table}

	// Table and column comments.
	if tc, ok := comments.tables[tableKey{schema: schema, table: table}]; ok {
		if err := applyTableComments(tableRoot, tc, change); err != nil {
			return nil, errors.E(op, fmt.Errorf("%s: %w", file, err))
		}
	}

	// Computed-field comments (inherited from the underlying function's DB comment).
	if err := applyComputedFieldComments(tableRoot, comments.functions, change); err != nil {
		return nil, errors.E(op, fmt.Errorf("%s: %w", file, err))
	}

	if !change.addTable && len(change.addCols) == 0 && len(change.addComputedFields) == 0 {
		return nil, nil
	}

	return change, nil
}

// applyTableComments mutates the table mapping node to add table/column comments
// that are "automatic" (absent or explicit null) in metadata, recording what was
// changed in `change`. It returns an error (so the whole run aborts before any
// write) if it would have to descend through a node it cannot edit safely, e.g. a
// 'configuration'/'column_config'/per-column entry that is an !include, a YAML
// alias, or any non-mapping node. Existing explicit comments (including "") and
// all other configuration are preserved.
func applyTableComments(tableRoot *yaml.Node, tc tableComments, change *tableCommentChange) error {
	// Determine, read-only, what needs adding before mutating anything.
	needTable := tc.table != "" &&
		isAutomaticComment(descend(tableRoot, "configuration", "comment"))

	var needCols []string

	for _, col := range sortedKeys(tc.columns) {
		if tc.columns[col] == "" {
			continue
		}

		if isAutomaticComment(
			descend(tableRoot, "configuration", "column_config", col, "comment"),
		) {
			needCols = append(needCols, col)
		}
	}

	if !needTable && len(needCols) == 0 {
		return nil
	}

	// YAML merge keys (<<) and aliases let a table's configuration inherit values
	// (including comments / custom_name) from elsewhere. mappingValue/descend do not
	// resolve them, so editing here could silently override an inherited explicit
	// comment. Refuse (before any write) if the configuration subtree uses either.
	if hasMergeOrAlias(mappingValue(tableRoot, "configuration")) {
		return stderrors.New(
			"cannot safely edit 'configuration': it uses a YAML merge key (<<) or alias; migrate this comment manually",
		)
	}

	cfg, err := editableChildMapping(tableRoot, "configuration")
	if err != nil {
		return err
	}

	if needTable {
		setMappingValue(cfg, "comment", scalarStringNode(tc.table))

		change.addTable = true
	}

	if len(needCols) > 0 {
		colCfg, err := editableChildMapping(cfg, "column_config")
		if err != nil {
			return err
		}

		for _, col := range needCols {
			colEntry, err := editableChildMapping(colCfg, col)
			if err != nil {
				return err
			}

			setMappingValue(colEntry, "comment", scalarStringNode(tc.columns[col]))
			change.addCols = append(change.addCols, col)
		}
	}

	return nil
}

// applyComputedFieldComments fills the 'comment' of a table's computed fields from
// the underlying function's database COMMENT, when the computed field's comment is
// automatic (absent or null). Only the computed field's 'comment' is touched;
// 'definition' and everything else are preserved. It aborts (before any write) if
// the computed field uses a YAML merge key/alias, or if the underlying function has
// multiple overloads (the comment cannot be attributed unambiguously, since
// function metadata does not encode the argument signature).
func applyComputedFieldComments(
	tableRoot *yaml.Node,
	functions map[tableKey]funcComment,
	change *tableCommentChange,
) error {
	cfSeq := mappingValue(tableRoot, "computed_fields")
	// Absent or explicit null means no computed fields.
	if cfSeq == nil || cfSeq.Tag == "!!null" {
		return nil
	}
	// Anything other than a plain list (e.g. an !include or alias) is a layout we
	// cannot edit safely; refuse rather than silently skip.
	if isIncludeNode(cfSeq) || cfSeq.Kind != yaml.SequenceNode {
		return fmt.Errorf(
			"cannot safely edit 'computed_fields': expected a list but found %s; migrate these comments manually",
			describeNode(cfSeq),
		)
	}

	for _, cf := range cfSeq.Content {
		if cf.Kind != yaml.MappingNode || isIncludeNode(cf) {
			return fmt.Errorf(
				"cannot safely edit a computed field: expected a mapping but found %s; migrate these comments manually",
				describeNode(cf),
			)
		}
		// definition / definition.function must be inspectable (not an alias/!include),
		// otherwise we cannot resolve the backing function safely.
		def := mappingValue(cf, "definition")
		if isIncludeNode(def) {
			return fmt.Errorf(
				"cannot safely edit a computed field: its 'definition' is %s; migrate its comment manually",
				describeNode(def),
			)
		}

		if fn := mappingValue(def, "function"); isIncludeNode(fn) {
			return fmt.Errorf(
				"cannot safely edit a computed field: its 'definition.function' is %s; migrate its comment manually",
				describeNode(fn),
			)
		}

		schema, name, ok := computedFieldFunctionIdentity(cf)
		if !ok {
			continue // not a function-backed computed field we can match
		}

		fc, ok := functions[tableKey{schema: schema, table: name}]
		if !ok {
			continue // no such function / no DB comment
		}

		if !isAutomaticComment(mappingValue(cf, "comment")) {
			continue // explicit comment (including "") is preserved
		}

		cfName := ""
		if n := mappingValue(cf, "name"); n != nil {
			cfName = n.Value
		}

		if fc.overloads > 1 {
			return fmt.Errorf(
				"computed field %q uses function %s.%s which has %d overloads; cannot determine which function's comment applies - set this computed field's comment manually",
				cfName,
				schema,
				name,
				fc.overloads,
			)
		}

		if fc.comment == "" {
			continue
		}

		if hasMergeOrAlias(cf) {
			return fmt.Errorf(
				"cannot safely edit computed field %q: it uses a YAML merge key or alias; migrate this comment manually",
				cfName,
			)
		}

		setMappingValue(cf, "comment", scalarStringNode(fc.comment))

		change.addComputedFields = append(change.addComputedFields, cfName)
	}

	return nil
}

// computedFieldFunctionIdentity extracts the (schema, name) of the function backing
// a computed field, from its 'definition.function' (a {schema, name} mapping or a
// bare function-name string; schema defaults to "public").
func computedFieldFunctionIdentity(cf *yaml.Node) (schema string, name string, ok bool) {
	fn := mappingValue(mappingValue(cf, "definition"), "function")
	if fn == nil {
		return "", "", false
	}

	switch fn.Kind {
	case yaml.ScalarNode:
		return "public", fn.Value, fn.Value != ""
	case yaml.MappingNode:
		nameNode := mappingValue(fn, "name")
		if nameNode == nil || nameNode.Value == "" {
			return "", "", false
		}

		schema = "public"
		if s := mappingValue(fn, "schema"); s != nil && s.Value != "" {
			schema = s.Value
		}

		return schema, nameNode.Value, true
	default:
		return "", "", false
	}
}

// planFunctionFileEdit reads one metadata file from the functions !include tree and
// stages a top-level 'comment' for any tracked function entries it contains.
func planFunctionFileEdit(
	source, file string,
	functions map[tableKey]funcComment,
) (*fileEdit, error) {
	var op errors.Op = "commands.planFunctionFileEdit"

	contents, err := os.ReadFile(file)
	if err != nil {
		return nil, errors.E(op, err)
	}

	var doc yaml.Node
	if err := yaml.Unmarshal(contents, &doc); err != nil {
		return nil, errors.E(op, fmt.Errorf("parsing %s: %w", file, err))
	}

	root := documentRoot(&doc)
	if root == nil {
		return nil, nil
	}

	// Collect the function-entry mapping nodes. Index files reference per-function
	// files via !include scalars (handled separately via those files), which we skip;
	// alias or other non-mapping entries are a layout we cannot edit safely.
	var funcNodes []*yaml.Node

	switch root.Kind {
	case yaml.MappingNode:
		if mappingValue(root, "function") != nil {
			funcNodes = append(funcNodes, root)
		}
	case yaml.SequenceNode:
		for _, entry := range root.Content {
			if isIncludeScalar(entry) {
				continue // legitimate index !include; the referenced file is handled on its own
			}

			if entry.Kind != yaml.MappingNode {
				return nil, errors.E(
					op,
					fmt.Errorf(
						"%s: cannot safely edit a function entry: expected a mapping but found %s; migrate its comment manually",
						file,
						describeNode(entry),
					),
				)
			}

			if mappingValue(entry, "function") != nil {
				funcNodes = append(funcNodes, entry)
			}
		}
	}

	var changes []funcCommentChange

	for _, fn := range funcNodes {
		change, err := planFunctionNode(source, file, fn, functions)
		if err != nil {
			return nil, errors.E(op, err)
		}

		if change != nil {
			changes = append(changes, *change)
		}
	}

	if len(changes) == 0 {
		return nil, nil
	}

	return &fileEdit{path: file, doc: &doc, funcChanges: changes}, nil
}

// planFunctionNode fills a tracked function's top-level 'comment' from its database
// COMMENT when the metadata comment is automatic (absent/null) and the function has
// exactly one overload with a nonempty comment. It aborts (before any write) on an
// ambiguous (multi-overload) match or an unsafe (merge/alias/!include) layout, and
// preserves explicit/"" comments and all other function metadata. Note: Hasura does
// not expose a tracked function's database COMMENT as its GraphQL description, so
// this moves the database text into metadata as a new query-root description (as
// requested) rather than restoring one that was previously rendered.
func planFunctionNode(
	source, file string,
	funcRoot *yaml.Node,
	functions map[tableKey]funcComment,
) (*funcCommentChange, error) {
	var op errors.Op = "commands.planFunctionNode"

	if fn := mappingValue(funcRoot, "function"); isIncludeNode(fn) {
		return nil, errors.E(
			op,
			fmt.Errorf(
				"%s: cannot safely edit a function entry: its 'function' is %s; migrate its comment manually",
				file,
				describeNode(fn),
			),
		)
	}

	schema, name, ok := functionIdentity(funcRoot)
	if !ok {
		return nil, errors.E(
			op,
			fmt.Errorf("%s: function entry has no usable 'function' identifier", file),
		)
	}

	fc, ok := functions[tableKey{schema: schema, table: name}]
	if !ok {
		return nil, nil // not a function we have a comment for
	}

	if !isAutomaticComment(mappingValue(funcRoot, "comment")) {
		return nil, nil // explicit comment (including "") is preserved
	}

	if fc.overloads > 1 {
		return nil, errors.E(
			op,
			fmt.Errorf(
				"%s: function %s.%s has %d overloads; cannot determine which function's comment applies - set this function's comment manually",
				file,
				schema,
				name,
				fc.overloads,
			),
		)
	}

	if fc.comment == "" {
		return nil, nil
	}

	if hasMergeOrAlias(funcRoot) {
		return nil, errors.E(
			op,
			fmt.Errorf(
				"%s: cannot safely edit function %s.%s: it uses a YAML merge key or alias; migrate this comment manually",
				file,
				schema,
				name,
			),
		)
	}

	setMappingValue(funcRoot, "comment", scalarStringNode(fc.comment))

	return &funcCommentChange{source: source, schema: schema, name: name}, nil
}

// functionIdentity extracts (schema, name) from a function metadata entry's
// 'function' field (a {schema, name} mapping or a bare function-name string; schema
// defaults to "public").
func functionIdentity(funcRoot *yaml.Node) (schema string, name string, ok bool) {
	fn := mappingValue(funcRoot, "function")
	if fn == nil {
		return "", "", false
	}

	switch fn.Kind {
	case yaml.ScalarNode:
		return "public", fn.Value, fn.Value != ""
	case yaml.MappingNode:
		nameNode := mappingValue(fn, "name")
		if nameNode == nil || nameNode.Value == "" {
			return "", "", false
		}

		schema = "public"
		if s := mappingValue(fn, "schema"); s != nil && s.Value != "" {
			schema = s.Value
		}

		return schema, nameNode.Value, true
	default:
		return "", "", false
	}
}

//------------------------------------------------------------------------------
// Writing changes

func writeFileEdits(edits []fileEdit) error {
	var op errors.Op = "commands.writeFileEdits"

	// Phase 1: encode every file (and stat its permissions). If any file fails to
	// encode, abort before writing anything.
	type encoded struct {
		path string
		data []byte
		perm fs.FileMode
	}

	var toWrite []encoded

	seen := map[string]bool{}
	for _, e := range edits {
		if seen[e.path] {
			continue
		}

		seen[e.path] = true

		data, err := encodeYAMLDocument(e.doc)
		if err != nil {
			return errors.E(op, fmt.Errorf("encoding %s: %w", e.path, err))
		}

		perm := fs.FileMode(0o644)
		if info, statErr := os.Stat(e.path); statErr == nil {
			perm = info.Mode().Perm()
		}

		toWrite = append(toWrite, encoded{path: e.path, data: data, perm: perm})
	}

	// Phase 2: write each file atomically (unique temp in the same dir + rename).
	for _, w := range toWrite {
		if err := atomicWriteFile(w.path, w.data, w.perm); err != nil {
			return errors.E(op, err)
		}
	}

	return nil
}

func encodeYAMLDocument(doc *yaml.Node) ([]byte, error) {
	var buf bytes.Buffer

	enc := yaml.NewEncoder(&buf)
	enc.SetIndent(2)

	if err := enc.Encode(doc); err != nil {
		return nil, err
	}

	if err := enc.Close(); err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}

func atomicWriteFile(path string, data []byte, perm fs.FileMode) error {
	var op errors.Op = "commands.atomicWriteFile"

	dir := filepath.Dir(path)

	tmp, err := os.CreateTemp(dir, "."+filepath.Base(path)+".codemod-*")
	if err != nil {
		return errors.E(op, err)
	}

	tmpName := tmp.Name()

	cleanup := true
	defer func() {
		if cleanup {
			_ = os.Remove(tmpName)
		}
	}()

	if _, err := tmp.Write(data); err != nil {
		_ = tmp.Close()

		return errors.E(op, err)
	}

	if err := tmp.Close(); err != nil {
		return errors.E(op, err)
	}

	if err := os.Chmod(tmpName, perm); err != nil {
		return errors.E(op, err)
	}

	if err := os.Rename(tmpName, path); err != nil {
		return errors.E(op, err)
	}

	cleanup = false

	return nil
}

//------------------------------------------------------------------------------
// Fetching comments from the database catalogs

const systemSchemaPredicate = `n.nspname NOT LIKE 'pg\_%' AND n.nspname NOT IN ('information_schema', 'hdb_catalog', 'hdb_lib', '_timescaledb_internal', 'crdb_internal')`

// tableCommentsSQL returns the catalog query for table COMMENTs. It takes no user
// input, so it is injection-safe; callers match the rows against tracked tables
// locally by exact (schema, table) name.
func tableCommentsSQL() string {
	return `SELECT n.nspname AS "schema", c.relname AS "table", d.description AS "comment"
FROM pg_catalog.pg_class c
JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
JOIN pg_catalog.pg_description d
  ON d.classoid = 'pg_catalog.pg_class'::regclass AND d.objoid = c.oid AND d.objsubid = 0
WHERE c.relkind IN ('r', 'v', 'm', 'p', 'f')
  AND d.description IS NOT NULL AND d.description <> ''
  AND ` + systemSchemaPredicate + `
ORDER BY n.nspname, c.relname;`
}

// columnCommentsSQL returns the catalog query for column COMMENTs.
func columnCommentsSQL() string {
	return `SELECT n.nspname AS "schema", c.relname AS "table", a.attname AS "column", d.description AS "comment"
FROM pg_catalog.pg_class c
JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped
JOIN pg_catalog.pg_description d
  ON d.classoid = 'pg_catalog.pg_class'::regclass AND d.objoid = c.oid AND d.objsubid = a.attnum
WHERE c.relkind IN ('r', 'v', 'm', 'p', 'f')
  AND d.description IS NOT NULL AND d.description <> ''
  AND ` + systemSchemaPredicate + `
ORDER BY n.nspname, c.relname, a.attnum;`
}

// functionCommentsSQL returns the catalog query for function COMMENTs. One row is
// returned per function overload (so callers can detect ambiguous (schema, name)
// pairs); the comment is coalesced to ” so the result always has three columns.
// Aggregates are excluded (they are not trackable functions). No user input.
func functionCommentsSQL() string {
	return `SELECT n.nspname AS "schema", p.proname AS "name", COALESCE(obj_description(p.oid, 'pg_proc'), '') AS "comment"
FROM pg_catalog.pg_proc p
JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
WHERE ` + systemSchemaPredicate + `
  AND NOT EXISTS (SELECT 1 FROM pg_catalog.pg_aggregate agg WHERE agg.aggfnoid = p.oid)
ORDER BY n.nspname, p.proname;`
}

// makeRunSQL returns a read-only run_sql caller bound to a source and its kind.
func makeRunSQL(
	v2 hasura.V2Query,
	src metadataSource,
) func(sql string) (*hasura.PGRunSQLOutput, error) {
	checkConsistency := false

	return func(sql string) (*hasura.PGRunSQLOutput, error) {
		in := hasura.PGRunSQLInput{
			SQL:                      sql,
			Source:                   src.name,
			ReadOnly:                 true,
			CheckMetadataConsistency: &checkConsistency,
		}
		switch src.kind {
		case hasura.SourceKindPG:
			return v2.PGRunSQL(in)
		case hasura.SourceKindCitus:
			out, err := v2.CitusRunSQL(hasura.CitusRunSQLInput(in))

			return (*hasura.PGRunSQLOutput)(out), err
		case hasura.SourceKindCockroach:
			out, err := v2.CockroachRunSQL(hasura.CockroachRunSQLInput(in))

			return (*hasura.PGRunSQLOutput)(out), err
		default:
			return nil, fmt.Errorf("unsupported source kind %q", src.kind)
		}
	}
}

// fetchSourceComments reads table, column and function comments from the database
// catalogs via the provided run_sql function. CockroachDB has neither column
// descriptions nor user functions, so only table comments are read for it.
func fetchSourceComments(
	kind hasura.SourceKind,
	runSQL func(sql string) (*hasura.PGRunSQLOutput, error),
) (*sourceComments, error) {
	var op errors.Op = "commands.fetchSourceComments"

	result := &sourceComments{
		tables:    map[tableKey]tableComments{},
		functions: map[tableKey]funcComment{},
	}

	tableOut, err := runSQL(tableCommentsSQL())
	if err != nil {
		return nil, errors.E(op, err)
	}

	tableRows, err := dataRows(tableOut, 3)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("table comment query: %w", err))
	}

	for _, row := range tableRows {
		key := tableKey{schema: row[0], table: row[1]}
		tc := result.tables[key]
		tc.table = row[2]
		result.tables[key] = tc
	}

	// CockroachDB never imported column descriptions and has no user functions, so
	// preserve that scope (and avoid running unsupported catalog queries against it).
	if kind != hasura.SourceKindCockroach {
		columnOut, err := runSQL(columnCommentsSQL())
		if err != nil {
			return nil, errors.E(op, err)
		}

		columnRows, err := dataRows(columnOut, 4)
		if err != nil {
			return nil, errors.E(op, fmt.Errorf("column comment query: %w", err))
		}

		for _, row := range columnRows {
			key := tableKey{schema: row[0], table: row[1]}

			tc := result.tables[key]
			if tc.columns == nil {
				tc.columns = map[string]string{}
			}

			tc.columns[row[2]] = row[3]
			result.tables[key] = tc
		}

		functionOut, err := runSQL(functionCommentsSQL())
		if err != nil {
			return nil, errors.E(op, err)
		}

		functionRows, err := dataRows(functionOut, 3)
		if err != nil {
			return nil, errors.E(op, fmt.Errorf("function comment query: %w", err))
		}
		// Each overload is a separate row; count overloads per (schema, name) and
		// remember the single overload's comment.
		overloadComments := map[tableKey][]string{}

		for _, row := range functionRows {
			key := tableKey{schema: row[0], table: row[1]}
			overloadComments[key] = append(overloadComments[key], row[2])
		}

		for key, comments := range overloadComments {
			fc := funcComment{overloads: len(comments)}
			if len(comments) == 1 {
				fc.comment = comments[0]
			}

			result.functions[key] = fc
		}
	}

	return result, nil
}

// dataRows validates a run_sql TuplesOk response and returns its data rows
// (dropping the header row). It errors on an unexpected result shape rather than
// silently reporting "no comments".
func dataRows(out *hasura.PGRunSQLOutput, cols int) ([][]string, error) {
	var op errors.Op = "commands.dataRows"
	if out == nil {
		return nil, errors.E(op, stderrors.New("run_sql returned no response"))
	}

	if out.ResultType != hasura.TuplesOK {
		return nil, errors.E(
			op,
			fmt.Errorf("expected result_type %q, got %q", hasura.TuplesOK, out.ResultType),
		)
	}

	if len(out.Result) == 0 {
		return nil, errors.E(op, stderrors.New("run_sql returned no header row"))
	}

	if len(out.Result[0]) != cols {
		return nil, errors.E(
			op,
			fmt.Errorf("expected %d columns, got %d", cols, len(out.Result[0])),
		)
	}

	rows := out.Result[1:]
	for i, row := range rows {
		if len(row) != cols {
			return nil, errors.E(
				op,
				fmt.Errorf("row %d has %d columns, expected %d", i, len(row), cols),
			)
		}
	}

	return rows, nil
}

//------------------------------------------------------------------------------
// yaml.Node helpers

func documentRoot(doc *yaml.Node) *yaml.Node {
	if doc == nil {
		return nil
	}

	if doc.Kind == yaml.DocumentNode {
		if len(doc.Content) == 0 {
			return nil
		}

		return doc.Content[0]
	}

	return doc
}

// mappingValue returns the value node for key in a mapping node, or nil. It only
// treats genuine mapping nodes as mappings (alias nodes are not followed).
func mappingValue(m *yaml.Node, key string) *yaml.Node {
	if m == nil || m.Kind != yaml.MappingNode {
		return nil
	}

	for i := 0; i+1 < len(m.Content); i += 2 {
		if m.Content[i].Value == key {
			return m.Content[i+1]
		}
	}

	return nil
}

// descend walks a chain of mapping keys, returning nil if any intermediate node
// is absent or not a mapping.
func descend(node *yaml.Node, keys ...string) *yaml.Node {
	cur := node
	for _, k := range keys {
		cur = mappingValue(cur, k)
		if cur == nil {
			return nil
		}
	}

	return cur
}

func setMappingValue(m *yaml.Node, key string, val *yaml.Node) {
	for i := 0; i+1 < len(m.Content); i += 2 {
		if m.Content[i].Value == key {
			m.Content[i+1] = val

			return
		}
	}

	m.Content = append(m.Content, scalarStringNode(key), val)
}

// editableChildMapping returns the mapping stored at key under parent, creating an
// empty mapping if the key is absent. It errors (rather than overwriting) if the
// key exists but is not a plain mapping node — e.g. an !include, a YAML alias, or
// any other node — so that custom layouts are never clobbered.
func editableChildMapping(parent *yaml.Node, key string) (*yaml.Node, error) {
	var op errors.Op = "commands.editableChildMapping"

	child := mappingValue(parent, key)
	if child == nil {
		child = &yaml.Node{Kind: yaml.MappingNode, Tag: "!!map"}
		setMappingValue(parent, key, child)

		return child, nil
	}
	// An explicit null (e.g. `configuration: null`) carries no data, so it is safe
	// to treat as an empty mapping — consistent with how the server treats it.
	if child.Kind == yaml.ScalarNode && child.Tag == "!!null" {
		child = &yaml.Node{Kind: yaml.MappingNode, Tag: "!!map"}
		setMappingValue(parent, key, child)

		return child, nil
	}

	if child.Kind != yaml.MappingNode || isIncludeNode(child) {
		return nil, errors.E(
			op,
			fmt.Errorf(
				"cannot safely edit %q: expected a mapping but found %s; migrate this comment manually",
				key,
				describeNode(child),
			),
		)
	}

	return child, nil
}

// hasMergeOrAlias reports whether the given node's subtree contains a YAML merge
// key (<<) or an alias node, anywhere. Such constructs make the resolved mapping
// depend on data defined elsewhere, which this codemod does not resolve.
func hasMergeOrAlias(node *yaml.Node) bool {
	if node == nil {
		return false
	}

	if node.Kind == yaml.AliasNode {
		return true
	}

	if node.Kind == yaml.MappingNode {
		for i := 0; i+1 < len(node.Content); i += 2 {
			k := node.Content[i]
			if k.Tag == "!!merge" || k.Value == "<<" {
				return true
			}

			if hasMergeOrAlias(node.Content[i+1]) {
				return true
			}
		}
	}

	if node.Kind == yaml.SequenceNode {
		if slices.ContainsFunc(node.Content, hasMergeOrAlias) {
			return true
		}
	}

	return false
}

// isIncludeScalar reports whether a node is specifically an !include scalar (a
// legitimate index reference), as opposed to a YAML alias.
func isIncludeScalar(n *yaml.Node) bool {
	return n != nil && n.Kind == yaml.ScalarNode &&
		(n.Tag == includeTag || strings.Contains(n.Value, includeTag))
}

func isIncludeNode(n *yaml.Node) bool {
	if n == nil {
		return false
	}

	if n.Kind == yaml.AliasNode {
		return true
	}

	if n.Tag == includeTag {
		return true
	}

	if n.Kind == yaml.ScalarNode && strings.Contains(n.Value, includeTag) {
		return true
	}

	return false
}

func describeNode(n *yaml.Node) string {
	if n == nil {
		return "nothing"
	}

	if n.Kind == yaml.AliasNode {
		return "a YAML alias"
	}

	if isIncludeNode(n) {
		return "an !include reference"
	}

	switch n.Kind {
	case yaml.ScalarNode:
		return "a scalar value"
	case yaml.SequenceNode:
		return "a list"
	default:
		return "an unexpected node"
	}
}

func scalarStringNode(s string) *yaml.Node {
	return &yaml.Node{Kind: yaml.ScalarNode, Tag: "!!str", Value: s}
}

// isAutomaticComment reports whether a comment value should be treated as
// "automatic" and therefore eligible to be filled from a database comment. A
// missing key or an explicit null is automatic; an explicit string (including the
// empty string "", which suppresses the description) is not.
func isAutomaticComment(commentNode *yaml.Node) bool {
	return commentNode == nil || commentNode.Tag == "!!null"
}

// tableIdentifier extracts the (schema, table) of a table metadata entry, whose
// "table" field is either a mapping {schema, name} or a bare table-name string
// (schema defaults to "public").
func tableIdentifier(tableRoot *yaml.Node) (schema string, table string, ok bool) {
	tableNode := mappingValue(tableRoot, "table")
	if tableNode == nil {
		return "", "", false
	}

	switch tableNode.Kind {
	case yaml.ScalarNode:
		return "public", tableNode.Value, tableNode.Value != ""
	case yaml.MappingNode:
		nameNode := mappingValue(tableNode, "name")
		if nameNode == nil || nameNode.Value == "" {
			return "", "", false
		}

		schema = "public"
		if schemaNode := mappingValue(
			tableNode,
			"schema",
		); schemaNode != nil &&
			schemaNode.Value != "" {
			schema = schemaNode.Value
		}

		return schema, nameNode.Value, true
	default:
		return "", "", false
	}
}

func sortedKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}

	sort.Strings(keys)

	return keys
}
