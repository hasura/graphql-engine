package source

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
)

var (
	ErrParse = fmt.Errorf("no match")
)

var (
	DefaultParse = Parse
	DefaultRegex = Regex
)

// Regex matches the following pattern:
//  123_name.up.ext
//  123_name.down.ext
var Regex = regexp.MustCompile(`^([0-9]+)_(.*)\.(` + string(Down) + `|` + string(Up) + `)\.(.*)$`)

// Parse returns Migration for matching Regex pattern.
func Parse(raw string) (*Migration, error) {
	var direction Direction
	m := Regex.FindStringSubmatch(raw)
	if len(m) == 5 {
		versionUint64, err := strconv.ParseUint(m[1], 10, 64)
		if err != nil {
			return nil, err
		}

		// Have different direction type for yaml and sql
		if m[4] == "yaml" {
			if m[3] == "up" {
				direction = MetaUp
			} else if m[3] == "down" {
				direction = MetaDown
			} else {
				return nil, errors.New("Invalid Direction type")
			}
		} else if m[4] == "sql" {
			if m[3] == "up" {
				direction = Up
			} else if m[3] == "down" {
				direction = Down
			} else {
				return nil, errors.New("Invalid Direction type")
			}
		}

		return &Migration{
			Version:    versionUint64,
			Identifier: m[2],
			Direction:  direction,
			Raw:        raw,
		}, nil
	}
	return nil, ErrParse
}
