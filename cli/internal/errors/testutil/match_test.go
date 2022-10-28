package testutil

import (
	"fmt"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func TestMatchType(t *testing.T) {
	tests := []struct {
		name    string
		err1    error
		err2    error
		wantErr bool
	}{
		{
			"err1 and err2 are of type *errors.Error",
			&errors.Error{
				Op: "errors.TestAssertType1",
			},
			&errors.Error{
				Op: "errors.TestAssertType2",
			},
			false,
		},
		{
			"only err1 is of type *errors.errorString",
			&errors.Error{
				Op: "errors.TestAssertType1",
			},
			fmt.Errorf("error"),
			true,
		},
		{
			"only err2 is of type *errors.errorString",
			fmt.Errorf("error"),
			&errors.Error{
				Op: "errors.TestAssertType1",
			},
			true,
		},
		{
			"neither is of type *errors.errorString",
			fmt.Errorf("error"),
			fmt.Errorf("error"),
			true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := matchType(tc.err1, tc.err2)
			if tc.wantErr {
				if got == nil {
					t.Fatalf("Expected error, got nil")
				} else if strings.HasPrefix(fmt.Sprint(got), "unexpected error type:") == false {
					t.Fatalf("Incorrect error: %s", got)
				}
			} else if !tc.wantErr && got != nil {
				t.Fatalf("Got unexpected error when wanted none: %s", got)
			}
		})
	}
}

func TestMatchValues(t *testing.T) {
	e := &errors.Error{
		Op:   "errors.TestAssertValues",
		Kind: errors.KindInternal,
		Err:  fmt.Errorf("some-random-error"),
	}

	tests := []struct {
		name    string
		err     *errors.Error
		wantErr bool
	}{
		{
			"is the same error",
			&errors.Error{
				Op:   "errors.TestAssertValues",
				Kind: errors.KindInternal,
				Err:  fmt.Errorf("some-random-error"),
			},
			false,
		},
		{
			"has different Op",
			&errors.Error{
				Op:   "errors.TestAssertValues-2",
				Kind: errors.KindInternal,
				Err:  fmt.Errorf("some-random-error"),
			},
			true,
		},
		{
			"has different Kind",
			&errors.Error{
				Op:   "errors.TestAssertValues",
				Kind: errors.KindHasuraAPI,
				Err:  fmt.Errorf("some-random-error"),
			},
			true,
		},
		{
			"has different Err",
			&errors.Error{
				Op:   "errors.TestAssertValues",
				Kind: errors.KindInternal,
				Err:  fmt.Errorf("some-random-error-2"),
			},
			true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := matchValues(e, tc.err)
			if tc.wantErr && got == nil {
				t.Fatalf("Expected error but got nil")
			} else if !tc.wantErr && got != nil {
				t.Fatalf("Got unexpected error when wanted none: %s", got)
			}
		})
	}
}
