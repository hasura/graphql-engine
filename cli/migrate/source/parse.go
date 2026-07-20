package source

import (
	stderrors "errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"

	"github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

var ErrParse = stderrors.New("no match")

var (
	DefaultParse   = Parse
	DefaultParsev2 = Parsev2
	DefaultRegex   = Regex
)

// Parser to parse source files.
type Parser func(raw string) (*Migration, error)

// Regex matches the following pattern:
//
//	123_name.up.ext
//	123_name.down.ext
var (
	Regex   = regexp.MustCompile(`^([0-9]+)_(.*)\.(` + string(Down) + `|` + string(Up) + `)\.(.*)$`)
	Regexv2 = regexp.MustCompile(
		`^([0-9]+)_(.*)\.(` + string(Down) + `|` + string(Up) + `)\.(sql)$`,
	)
)

// Parse returns Migration for matching Regex pattern.
func Parse(raw string) (*Migration, error) {
	var (
		op        errors.Op = "source.Parse"
		direction Direction
	)

	m := Regex.FindStringSubmatch(raw)
	if len(m) == 5 {
		versionUint64, err := strconv.ParseUint(m[1], 10, 64)
		if err != nil {
			return nil, errors.E(op, err)
		}

		// Have different direction type for yaml and sql
		switch m[4] {
		case "yaml":
			switch m[3] {
			case "up":
				direction = MetaUp
			case "down":
				direction = MetaDown
			default:
				return nil, errors.E(op, "Invalid Direction type")
			}
		case "sql":
			switch m[3] {
			case "up":
				direction = Up
			case "down":
				direction = Down
			default:
				return nil, errors.E(op, "Invalid Direction type")
			}
		}

		return &Migration{
			Version:    versionUint64,
			Identifier: m[2],
			Direction:  direction,
		}, nil
	}

	return nil, errors.E(op, ErrParse)
}

// Parsev2 returns Migration for matching Regex (only sql) pattern.
func Parsev2(raw string) (*Migration, error) {
	var (
		op        errors.Op = "source.Parsev2"
		direction Direction
	)

	m := Regexv2.FindStringSubmatch(raw)
	if len(m) == 5 {
		versionUint64, err := strconv.ParseUint(m[1], 10, 64)
		if err != nil {
			return nil, errors.E(op, err)
		}

		// Have different direction type for sql
		if m[4] == "sql" {
			switch m[3] {
			case "up":
				direction = Up
			case "down":
				direction = Down
			default:
				return nil, errors.E(op, "Invalid Direction type")
			}
		}

		return &Migration{
			Version:    versionUint64,
			Identifier: m[2],
			Direction:  direction,
		}, nil
	}

	return nil, errors.E(op, ErrParse)
}

// Validate file to check for empty sql or yaml content.
func IsEmptyFile(m *Migration, directory string) (bool, error) {
	var op errors.Op = "source.IsEmptyFile"

	data, err := os.ReadFile(filepath.Join(directory, m.Raw))
	if err != nil {
		return false, errors.E(op, fmt.Errorf("cannot read file %s: %w", m.Raw, err))
	}

	switch direction := m.Direction; direction {
	case MetaUp, MetaDown:
		var t []any

		err = yaml.Unmarshal(data, &t)
		if err != nil {
			return false, errors.E(op, fmt.Errorf("invalid yaml file: %s: %w", m.Raw, err))
		}

		if len(t) == 0 {
			return false, nil
		}
	case Up, Down:
		if string(data) == "" {
			return false, nil
		}
	}

	return true, nil
}
