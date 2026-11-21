package seed

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	internalerrors "github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/spf13/afero"
)

// CreateSeedOpts has the list of options required
// to create a seed file.
type CreateSeedOpts struct {
	UserProvidedSeedName string
	// DirectoryPath in which seed file should be created
	DirectoryPath string
	Data          io.Reader
	Database      string
}

// CreateSeedFile creates a .sql file according to the arguments
// it'll return full filepath and an error if any.
func CreateSeedFile(fs afero.Fs, opts CreateSeedOpts) (*string, error) {
	var op internalerrors.Op = "seed.CreateSeedFile"
	if opts.Data == nil {
		return nil, internalerrors.E(op, errors.New("no data provided"))
	}

	const fileExtension = "sql"

	timestamp := strconv.FormatInt(time.Now().UnixNano()/int64(time.Millisecond), 10)
	// filename will be in format <timestamp>_<userProvidedSeedName>.sql
	filenameWithTimeStamp := fmt.Sprintf(
		"%s_%s.%s",
		timestamp,
		opts.UserProvidedSeedName,
		fileExtension,
	)
	fullFilePath := filepath.Join(
		filepath.Join(opts.DirectoryPath, opts.Database),
		filenameWithTimeStamp,
	)

	// Write contents to file
	file, err := fs.OpenFile(fullFilePath, os.O_WRONLY|os.O_CREATE, 0o644)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	defer file.Close()

	r := bufio.NewReader(opts.Data)

	_, err = io.Copy(file, r)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}

	return &fullFilePath, nil
}

func (d *Driver) ExportDatadump(tableNames []string, sourceName string) (io.Reader, error) {
	var op internalerrors.Op = "seed.Driver.ExportDatadump"
	// to support tables starting with capital letters
	modifiedTableNames := make([]string, len(tableNames))

	for idx, val := range tableNames {
		split := strings.Split(val, ".")
		splitLen := len(split)

		if splitLen != 1 && splitLen != 2 {
			return nil, internalerrors.E(op, fmt.Errorf(`invalid schema/table provided "%s"`, val))
		}

		if splitLen == 2 {
			modifiedTableNames[idx] = fmt.Sprintf(`"%s"."%s"`, split[0], split[1])
		} else {
			modifiedTableNames[idx] = fmt.Sprintf(`"%s"`, val)
		}
	}

	pgDumpOpts := []string{"--no-owner", "--no-acl", "--data-only", "--column-inserts"}
	for _, table := range modifiedTableNames {
		pgDumpOpts = append(pgDumpOpts, "--table", table)
	}

	// clean_output is not used here: the server drops every blank line and
	// every line starting with "--", including lines inside multi-line text
	// values, which corrupts the seed data. The output is cleaned below with
	// the same rules, applied only to lines that start outside a literal.
	request := hasura.PGDumpRequest{
		Opts:        pgDumpOpts,
		CleanOutput: false,
		SourceName:  sourceName,
	}

	response, err := d.PGDumpClient.Send(request)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}

	dump, err := io.ReadAll(response)
	if err != nil {
		return nil, internalerrors.E(op, fmt.Errorf("reading pg_dump response: %w", err))
	}

	return strings.NewReader(cleanDataDump(string(dump))), nil
}

// preambleLines are the session settings pg_dump writes at the top of a dump.
// It is the list the server drops when clean_output is set.
var preambleLines = map[string]bool{
	"SET statement_timeout = 0;":                              true,
	"SET lock_timeout = 0;":                                   true,
	"SET idle_in_transaction_session_timeout = 0;":            true,
	"SET client_encoding = 'UTF8';":                           true,
	"SET standard_conforming_strings = on;":                   true,
	"SELECT pg_catalog.set_config('search_path', '', false);": true,
	"SET xmloption = content;":                                true,
	"SET client_min_messages = warning;":                      true,
	"SET row_security = off;":                                 true,
	"SET default_tablespace = '';":                            true,
	"SET default_with_oids = false;":                          true,
	"SET default_table_access_method = heap;":                 true,
	"CREATE SCHEMA public;":                                   true,
	"COMMENT ON SCHEMA public IS 'standard public schema';":   true,
}

// cleanDataDump removes blank lines, "--" comments and the preamble from a
// pg_dump data dump, like the server does for clean_output, but keeps every
// line that starts inside a quoted string or identifier, so that blank lines
// and lines starting with "--" in multi-line text values are preserved.
func cleanDataDump(dump string) string {
	lines := strings.Split(dump, "\n")
	if len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}

	var (
		out      strings.Builder
		inSingle bool
		inDouble bool
	)

	for _, line := range lines {
		inLiteral := inSingle || inDouble
		if !inLiteral && (strings.TrimSpace(line) == "" ||
			strings.HasPrefix(line, "--") ||
			preambleLines[line]) {
			continue
		}

		out.WriteString(line)
		out.WriteString("\n")

		// pg_dump writes literals with standard_conforming_strings on, so
		// quotes are escaped by doubling them, which toggles the state twice
		for _, c := range line {
			switch {
			case c == '\'' && !inDouble:
				inSingle = !inSingle
			case c == '"' && !inSingle:
				inDouble = !inDouble
			}
		}
	}

	return out.String()
}
