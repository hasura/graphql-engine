package diff

import (
	"fmt"
	"io"
	"path/filepath"
	"strings"

	"github.com/mgutz/ansi"

	"github.com/hexops/gotextdiff"
	"github.com/hexops/gotextdiff/myers"
	"github.com/hexops/gotextdiff/span"

	"github.com/gonvenience/ytbx"
	"github.com/homeport/dyff/pkg/dyff"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func YamlDiff(fromFile, toFile ytbx.InputFile, writer io.Writer, fileName string) (int, error) {
	var op errors.Op = "diff.YamlDiff"
	report, err := dyff.CompareInputFiles(fromFile, toFile, dyff.IgnoreOrderChanges(true))
	if err != nil {
		return -1, errors.E(op, fmt.Errorf("error while getting diff: %w", err))
	}
	reportWriter := &dyff.HumanReport{
		Report:            report,
		DoNotInspectCerts: false,
		NoTableStyle:      false,
		OmitHeader:        true,
		UseGoPatchPaths:   false,
	}
	if len(report.Diffs) != 0 {
		fmt.Fprintf(writer, "%s\n", fileName)
		err = reportWriter.WriteReport(writer)
		if err != nil {
			return -1, errors.E(op, fmt.Errorf("error while printing diff: %w", err))
		}
	}
	return len(report.Diffs), nil
}

func IsYAMLFile(filename string) bool {
	return filepath.Ext(filename) == ".yaml" || filepath.Ext(filename) == ".yml"
}

func MyersDiff(before, after, from, to string, writer io.Writer, disableColor bool) (int, error) {
	edits := myers.ComputeEdits(span.URIFromPath("a.txt"), before, after)
	text := fmt.Sprint(gotextdiff.ToUnified(from, to, before, edits))

	lines := strings.Split(text, "\n")
	if len(lines) <= 2 {
		return 0, nil
	}
	for _, line := range lines[2:] {
		if line == "" {
			break
		}
		if (string)(line[0]) == "-" {
			fmt.Fprintf(writer, "%s\n", MakeDiffLine(line, "red", disableColor))
		} else if (string)(line[0]) == "+" {
			fmt.Fprintf(writer, "%s\n", MakeDiffLine(line, "green", disableColor))
		} else if (string)(line[0]) == "@" {
			fmt.Fprintf(writer, "%s\n", MakeDiffLine(line, "cyan", disableColor))
		}
	}
	return len(lines), nil
}

func MakeDiffLine(line, color string, disableColor bool) string {
	if disableColor {
		return line
	}
	return ansi.Color(line, color)
}
