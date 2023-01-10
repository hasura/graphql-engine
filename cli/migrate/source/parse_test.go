package source

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func TestParse(t *testing.T) {
	tt := []struct {
		name            string
		wantErr         bool
		assertErr       require.ErrorAssertionFunc
		expectMigration *Migration
	}{
		{
			name:      "1_foobar.up.sql",
			wantErr:   false,
			assertErr: require.NoError,
			expectMigration: &Migration{
				Version:    1,
				Identifier: "foobar",
				Direction:  Up,
			},
		},
		{
			name:      "1_foobar.up.yaml",
			wantErr:   false,
			assertErr: require.NoError,
			expectMigration: &Migration{
				Version:    1,
				Identifier: "foobar",
				Direction:  MetaUp,
			},
		},
		{
			name:      "1_foobar.down.sql",
			wantErr:   false,
			assertErr: require.NoError,
			expectMigration: &Migration{
				Version:    1,
				Identifier: "foobar",
				Direction:  Down,
			},
		},
		{
			name:      "1_foobar.down.yaml",
			wantErr:   false,
			assertErr: require.NoError,
			expectMigration: &Migration{
				Version:    1,
				Identifier: "foobar",
				Direction:  MetaDown,
			},
		},
		{
			name:      "1_f-o_ob+ar.up.sql",
			wantErr:   false,
			assertErr: require.NoError,
			expectMigration: &Migration{
				Version:    1,
				Identifier: "f-o_ob+ar",
				Direction:  Up,
			},
		},
		{
			name:      "1485385885_foobar.up.sql",
			wantErr:   false,
			assertErr: require.NoError,
			expectMigration: &Migration{
				Version:    1485385885,
				Identifier: "foobar",
				Direction:  Up,
			},
		},
		{
			name:      "20170412214116_date_foobar.up.sql",
			wantErr:   false,
			assertErr: require.NoError,
			expectMigration: &Migration{
				Version:    20170412214116,
				Identifier: "date_foobar",
				Direction:  Up,
			},
		},
		{
			name:    "-1_foobar.up.sql",
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, e.Err, ErrParse)
			}),
			expectMigration: nil,
		},
		{
			name:    "foobar.up.sql",
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, e.Err, ErrParse)
			}),
			expectMigration: nil,
		},
		{
			name:    "1.up.sql",
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, e.Err, ErrParse)
			}),
			expectMigration: nil,
		},
		{
			name:    "1_foobar.sql",
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, e.Err, ErrParse)
			}),
			expectMigration: nil,
		},
		{
			name:    "1_foobar.up",
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, e.Err, ErrParse)
			}),
			expectMigration: nil,
		},
		{
			name:    "1_foobar.down",
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, e.Err, ErrParse)
			}),
			expectMigration: nil,
		},
	}

	for i, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			f, err := Parse(tc.name)
			tc.assertErr(t, err)
			if tc.wantErr {
				return
			}
			if *f != *tc.expectMigration {
				t.Errorf("expected %+v, got %+v, in %v", *tc.expectMigration, *f, i)
			}
		})
	}
}
