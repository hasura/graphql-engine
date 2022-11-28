package testutil

import (
	"fmt"
	"io"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/olekukonko/tablewriter"
)

func Match(t *testing.T, out io.Writer, want, got error) {
	t.Helper()
	if got == nil && want == nil {
		return
	}
	if got == nil || want == nil {
		t.Fatalf("Wanted *errors.Error, got nil")
	}
	if err := matchType(want, got); err != nil {
		t.Fatal(err)
	}
	if err := matchValues(want, got); err != nil {
		printDiff(out, want.(*errors.Error), got.(*errors.Error))
		t.Fatal(err)
	}
}

func matchType(err1, err2 error) error {
	_, ok := err1.(*errors.Error)
	if !ok {
		return fmt.Errorf("unexpected error type: Wanted error type: *errors.Error; Got error type: %T", err1)
	}
	_, ok = err2.(*errors.Error)
	if !ok {
		return fmt.Errorf("unexpected error type: Wanted error type: *errors.Error; Got error type: %T", err2)
	}
	return nil
}

func matchValues(err1, err2 error) error {
	e1 := err1.(*errors.Error)
	e2 := err2.(*errors.Error)
	if e1.Op != e2.Op || e1.Kind != e2.Kind || e1.Error() != e2.Error() {
		return fmt.Errorf("expected a different error, please check the above table")
	}
	return nil
}

func printDiff(out io.Writer, want, got *errors.Error) {
	data := [][]string{
		{"Op", string(want.Op), string(got.Op)},
		{"Kind", want.Kind.String(), got.Kind.String()},
		{"Error", want.Error(), got.Error()},
	}
	table := tablewriter.NewWriter(out)
	table.SetHeader([]string{"", "Want", "Got"})
	table.SetRowLine(true)
	for _, v := range data {
		table.Append(v)
	}
	table.Render()
}
