package testutil

import (
	stderrors "errors"
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

	err := matchType(want, got)
	if err != nil {
		t.Fatal(err)
	}

	err = matchValues(want, got)
	if err != nil {
		printDiff(out, func() *errors.Error {
			target := &errors.Error{}
			_ = stderrors.As(want, &target)

			return target
		}(), func() *errors.Error {
			target := &errors.Error{}
			_ = stderrors.As(got, &target)

			return target
		}())
		t.Fatal(err)
	}
}

func matchType(err1, err2 error) error {
	intError := &errors.Error{}

	ok := stderrors.As(err1, &intError)
	if !ok {
		return fmt.Errorf(
			"unexpected error type: Wanted error type: *errors.Error; Got error type: %T",
			err1,
		)
	}

	ok = stderrors.As(err2, &intError)
	if !ok {
		return fmt.Errorf(
			"unexpected error type: Wanted error type: *errors.Error; Got error type: %T",
			err2,
		)
	}

	return nil
}

func matchValues(err1, err2 error) error {
	e1 := func() *errors.Error {
		target := &errors.Error{}
		_ = stderrors.As(err1, &target)

		return target
	}()

	e2 := func() *errors.Error {
		target := &errors.Error{}
		_ = stderrors.As(err2, &target)

		return target
	}()
	if e1.Op != e2.Op || e1.Kind != e2.Kind || e1.Error() != e2.Error() {
		return stderrors.New("expected a different error, please check the above table")
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
	table.Header([]string{"", "Want", "Got"})

	for _, v := range data {
		table.Append(v)
	}

	table.Render()
}
