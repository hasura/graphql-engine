package migrate

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
)

func TestProjectMigrate_ApplyConfig_v3(t *testing.T) {
	port, teardown := testutil.StartHasuraWithMetadataDatabase(t, testutil.HasuraDockerImage)
	hasuraEndpoint := fmt.Sprintf("%s:%s", testutil.BaseURL, port)
	connectionStringSource1, teardownPG1 := testutil.StartPGContainer(t)
	connectionStringSource2, teardownPG2 := testutil.StartPGContainer(t)
	testutil.AddPGSourceToHasura(t, hasuraEndpoint, connectionStringSource1, "s1")
	testutil.AddPGSourceToHasura(t, hasuraEndpoint, connectionStringSource2, "s2")
	defer func() {
		teardownPG2()
		teardownPG1()
		teardown()
	}()
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	type fields struct {
		projectDirectory string
		endpointString   string
	}
	type args struct {
		opts []ProjectMigrationApplierOption
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []ApplyResult
		wantErr bool
	}{
		{
			"can apply migrations in config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
				endpointString:   hgeEndpoint,
			},
			args{
				[]ProjectMigrationApplierOption{ApplyOnAllDatabases()},
			},
			[]ApplyResult{
				{
					"s1",
					"migrations applied on database: s1",
					nil,
				},
				{
					"s2",
					"migrations applied on database: s2",
					nil,
				},
			},
			false,
		},
		{
			"can apply a version in config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
				endpointString:   hgeEndpoint,
			},
			args{
				[]ProjectMigrationApplierOption{ApplyOnDatabaseName("s1"), ApplyVersion("1623841477474", MigrationDirectionDown)},
			},
			[]ApplyResult{
				{
					"s1",
					"migrations applied",
					nil,
				},
			},
			false,
		},
		{
			"can apply a version in config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
				endpointString:   hgeEndpoint,
			},
			args{
				[]ProjectMigrationApplierOption{ApplyOnDatabaseName("s1"), ApplyVersion("1623841477474", MigrationDirectionUp)},
			},
			[]ApplyResult{
				{
					"s1",
					"migrations applied",
					nil,
				},
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMigrate(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			got, err := p.Apply(tt.args.opts...)
			if tt.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				require.Equal(t, tt.want, got)
			}
		})
	}
}

func TestProjectMigrate_Apply_Configv2(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	type fields struct {
		projectDirectory string
		adminSecret      string
		endpointString   string
	}
	type args struct {
		opts []ProjectMigrationApplierOption
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []ApplyResult
		wantErr bool
	}{
		{
			"can apply migrations in config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
				adminSecret:      "",
				endpointString:   hgeEndpoint,
			},
			args{
				[]ProjectMigrationApplierOption{ApplyOnAllDatabases()},
			},
			[]ApplyResult{
				{
					Message: "migrations applied",
				},
			},
			false,
		},
		{
			"can apply down migration on a version in config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
				adminSecret:      "",
				endpointString:   hgeEndpoint,
			},
			args{
				[]ProjectMigrationApplierOption{ApplyVersion("1623842054907", MigrationDirectionDown)},
			},
			[]ApplyResult{
				{
					Message: "migrations applied",
				},
			},
			false,
		},
		{
			"throws error when trying to do a down migration which is not applied",
			fields{
				projectDirectory: "testdata/projectv2",
				adminSecret:      "",
				endpointString:   hgeEndpoint,
			},
			args{
				[]ProjectMigrationApplierOption{ApplyVersion("1623842054907", MigrationDirectionDown)},
			},
			[]ApplyResult{
				{
					Error: fmt.Errorf("skipping applying migrations on database , encountered: \nMigration not applied in database"),
				},
			},
			false,
		},
		{
			"can apply up migrations of a version on a config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
				adminSecret:      "",
				endpointString:   hgeEndpoint,
			},
			args{
				[]ProjectMigrationApplierOption{ApplyVersion("1623842054907", MigrationDirectionUp)},
			},
			[]ApplyResult{
				{
					Message: "migrations applied",
				},
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMigrate(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			got, err := p.Apply(tt.args.opts...)
			if tt.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				for idx, want := range tt.want {
					if idx >= len(got) {
						t.Errorf("expected to got to have equal number of elements: want %v got %v", len(tt.want), len(got))
					}
					if len(want.Message) > 0 {
						assert.Equal(t, want.Message, got[idx].Message)
					}
					if want.Error != nil {
						assert.Equal(t, want.Error.Error(), got[idx].Error.Error())
					}
				}
			}
		})
	}
}

func TestProjectMigrate_Status_ConfigV2(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	type fields struct {
		projectDirectory string
		adminSecret      string
		endpointString   string
	}
	type args struct {
		opts []ProjectMigrationStatusOption
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    string
		wantErr bool
		before  func(t *testing.T, p *ProjectMigrate)
	}{
		{
			"can get status of migrations",
			fields{
				projectDirectory: "testdata/projectv2",
				adminSecret:      "",
				endpointString:   hgeEndpoint,
			},
			args{
				opts: []ProjectMigrationStatusOption{},
			},
			`
[
  {
    "databaseName": "default",
    "status": {
      "migrations": [
        1623842054907,
        1623842062104,
        1623842069725,
        1623842076537,
        1623842087940
      ],
      "status": {
        "1623842054907": {
          "database_status": false,
          "source_status": true
        },
        "1623842062104": {
          "database_status": false,
          "source_status": true
        },
        "1623842069725": {
          "database_status": false,
          "source_status": true
        },
        "1623842076537": {
          "database_status": false,
          "source_status": true
        },
        "1623842087940": {
          "database_status": false,
          "source_status": true
        }
      }
    }
  }
]`,
			false,
			func(t *testing.T, p *ProjectMigrate) {},
		},
		{
			"can get status of migrations",
			fields{
				projectDirectory: "testdata/projectv2",
				adminSecret:      "",
				endpointString:   hgeEndpoint,
			},
			args{
				opts: []ProjectMigrationStatusOption{},
			},
			`
[
  {
    "databaseName": "default",
    "status": {
      "migrations": [
        1623842054907,
        1623842062104,
        1623842069725,
        1623842076537,
        1623842087940
      ],
      "status": {
        "1623842054907": {
          "database_status": true,
          "source_status": true
        },
        "1623842062104": {
          "database_status": true,
          "source_status": true
        },
        "1623842069725": {
          "database_status": true,
          "source_status": true
        },
        "1623842076537": {
          "database_status": true,
          "source_status": true
        },
        "1623842087940": {
          "database_status": true,
          "source_status": true
        }
      }
    }
  }
]`,
			false,
			func(t *testing.T, p *ProjectMigrate) {
				_, err := p.Apply(ApplyOnAllDatabases())
				assert.NoError(t, err)
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMigrate(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			applier, err := NewProjectMigrate(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			tt.before(t, applier)
			got, err := p.status(tt.args.opts...)
			if tt.wantErr {
				require.Error(t, err)
			}
			require.NoError(t, err)
			gotJSON, err := json.Marshal(got)
			require.NoError(t, err)
			require.JSONEq(t, tt.want, string(gotJSON))

			statusJson, err := p.StatusJSON(tt.args.opts...)
			require.NoError(t, err)
			statusJsonb, err := ioutil.ReadAll(statusJson)
			require.NoError(t, err)
			require.JSONEq(t, tt.want, string(statusJsonb))
		})
	}
}

func TestProjectMigrate_Status_ConfigV3(t *testing.T) {
	port, teardown := testutil.StartHasuraWithMetadataDatabase(t, testutil.HasuraDockerImage)
	hasuraEndpoint := fmt.Sprintf("%s:%s", testutil.BaseURL, port)
	connectionStringSource1, teardownPG1 := testutil.StartPGContainer(t)
	connectionStringSource2, teardownPG2 := testutil.StartPGContainer(t)
	testutil.AddPGSourceToHasura(t, hasuraEndpoint, connectionStringSource1, "s1")
	testutil.AddPGSourceToHasura(t, hasuraEndpoint, connectionStringSource2, "s2")
	defer func() {
		teardownPG2()
		teardownPG1()
		teardown()
	}()
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	type fields struct {
		projectDirectory string
		adminSecret      string
	}
	type args struct {
		opts []ProjectMigrationStatusOption
	}
	tests := []struct {
		name      string
		fields    fields
		args      args
		want      string
		wantErr   bool
		testSetup func() (hgeEndpoint string, teardown func())
		before    func(t *testing.T, p *ProjectMigrate)
	}{
		{
			"can get status of migrations",
			fields{
				projectDirectory: "testdata/projectv3",
				adminSecret:      "",
			},
			args{
				opts: []ProjectMigrationStatusOption{},
			},
			`[
  {
    "databaseName": "s1",
    "status": {
      "migrations": [
        1623841477474,
        1623841485323,
        1623841492743,
        1623841500466,
        1623841510619
      ],
      "status": {
        "1623841477474": {
          "database_status": false,
          "source_status": true
        },
        "1623841485323": {
          "database_status": false,
          "source_status": true
        },
        "1623841492743": {
          "database_status": false,
          "source_status": true
        },
        "1623841500466": {
          "database_status": false,
          "source_status": true
        },
        "1623841510619": {
          "database_status": false,
          "source_status": true
        }
      }
    }
  },
  {
    "databaseName": "s2",
    "status": {
      "migrations": [
        1623841477474,
        1623841485323,
        1623841492743,
        1623841500466,
        1623841510619
      ],
      "status": {
        "1623841477474": {
          "database_status": false,
          "source_status": true
        },
        "1623841485323": {
          "database_status": false,
          "source_status": true
        },
        "1623841492743": {
          "database_status": false,
          "source_status": true
        },
        "1623841500466": {
          "database_status": false,
          "source_status": true
        },
        "1623841510619": {
          "database_status": false,
          "source_status": true
        }
      }
    }
  }
]`,
			false,
			func() (string, func()) { return hgeEndpoint, func() {} },
			func(t *testing.T, p *ProjectMigrate) {},
		},
		{
			"can get status of migrations",
			fields{
				projectDirectory: "testdata/projectv3",
				adminSecret:      "",
			},
			args{
				opts: []ProjectMigrationStatusOption{},
			},
			`
[
  {
    "databaseName": "s1",
    "status": {
      "migrations": [
        1623841477474,
        1623841485323,
        1623841492743,
        1623841500466,
        1623841510619
      ],
      "status": {
        "1623841477474": {
          "database_status": true,
          "source_status": true
        },
        "1623841485323": {
          "database_status": true,
          "source_status": true
        },
        "1623841492743": {
          "database_status": true,
          "source_status": true
        },
        "1623841500466": {
          "database_status": true,
          "source_status": true
        },
        "1623841510619": {
          "database_status": true,
          "source_status": true
        }
      }
    }
  },
  {
    "databaseName": "s2",
    "status": {
      "migrations": [
        1623841477474,
        1623841485323,
        1623841492743,
        1623841500466,
        1623841510619
      ],
      "status": {
        "1623841477474": {
          "database_status": true,
          "source_status": true
        },
        "1623841485323": {
          "database_status": true,
          "source_status": true
        },
        "1623841492743": {
          "database_status": true,
          "source_status": true
        },
        "1623841500466": {
          "database_status": true,
          "source_status": true
        },
        "1623841510619": {
          "database_status": true,
          "source_status": true
        }
      }
    }
  }
]`,
			false,
			func() (string, func()) { return hgeEndpoint, func() {} },
			func(t *testing.T, p *ProjectMigrate) {
				_, err := p.Apply(ApplyOnAllDatabases())
				assert.NoError(t, err)
			},
		},
		{
			"can throw an error when no databases are connected to hge",
			fields{
				projectDirectory: "testdata/projectv3",
				adminSecret:      "",
			},
			args{
				opts: []ProjectMigrationStatusOption{},
			},
			``,
			true,
			func() (string, func()) {
				port, teardown := testutil.StartHasuraWithMetadataDatabase(t, testutil.HasuraDockerImage)
				return fmt.Sprintf("http://%s:%s", testutil.Hostname, port), teardown
			},
			func(t *testing.T, p *ProjectMigrate) {
				_, err := p.Apply(ApplyOnAllDatabases())
				assert.NoError(t, err)
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hgeEndpoint, setupTeardown := tt.testSetup()
			defer setupTeardown()
			p, err := NewProjectMigrate(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(hgeEndpoint))
			require.NoError(t, err)
			applier, err := NewProjectMigrate(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(hgeEndpoint))
			require.NoError(t, err)
			tt.before(t, applier)
			got, err := p.status(tt.args.opts...)
			if tt.wantErr {
				require.Error(t, err)
			} else {
				gotJSON, err := json.Marshal(got)
				require.NoError(t, err)
				require.JSONEq(t, tt.want, string(gotJSON))

				statusJson, err := p.StatusJSON(tt.args.opts...)
				require.NoError(t, err)
				statusJsonb, err := ioutil.ReadAll(statusJson)
				require.NoError(t, err)
				require.JSONEq(t, tt.want, string(statusJsonb))
			}
		})
	}
}

func TestProjectMigrate_SkipExecution_Configv3(t *testing.T) {
	port, teardown := testutil.StartHasuraWithMetadataDatabase(t, testutil.HasuraDockerImage)
	hasuraEndpoint := fmt.Sprintf("%s:%s", testutil.BaseURL, port)
	connectionStringSource1, teardownPG1 := testutil.StartPGContainer(t)
	testutil.AddPGSourceToHasura(t, hasuraEndpoint, connectionStringSource1, "s1")
	defer func() {
		teardownPG1()
		teardown()
	}()
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	p, err := NewProjectMigrate("testdata/projectv3", WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(hgeEndpoint))
	require.NoError(t, err)
	_, err = p.Apply(ApplyOnAllDatabases())
	require.NoError(t, err)

	type args struct {
		opts []ProjectMigrationApplierOption
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{
			"mark migration as unapplied",
			args{
				opts: []ProjectMigrationApplierOption{
					ApplyOnDatabaseName("s1"), ApplyVersion("1623841477474", MigrationDirectionDown), ApplyWithSkipExecution(),
				},
			},
			`
				[
					{
						"databaseName": "s1",
						"status": {
							"migrations": [
								1623841477474,
								1623841485323,
								1623841492743,
								1623841500466,
								1623841510619
							],
							"status": {
								"1623841477474": {
									"database_status": false,
									"source_status": true
								},
								"1623841485323": {
									"database_status": true,
									"source_status": true
								},
								"1623841492743": {
									"database_status": true,
									"source_status": true
								},
								"1623841500466": {
									"database_status": true,
									"source_status": true
								},
								"1623841510619": {
									"database_status": true,
									"source_status": true
								}
							}
						}
					}
				]
				`,
		},
		{
			"mark migration as applied",
			args{
				opts: []ProjectMigrationApplierOption{
					ApplyOnDatabaseName("s1"), ApplyVersion("1623841477474", MigrationDirectionUp), ApplyWithSkipExecution(),
				},
			},
			`
				[
					{
						"databaseName": "s1",
						"status": {
							"migrations": [
								1623841477474,
								1623841485323,
								1623841492743,
								1623841500466,
								1623841510619
							],
							"status": {
								"1623841477474": {
									"database_status": true,
									"source_status": true
								},
								"1623841485323": {
									"database_status": true,
									"source_status": true
								},
								"1623841492743": {
									"database_status": true,
									"source_status": true
								},
								"1623841500466": {
									"database_status": true,
									"source_status": true
								},
								"1623841510619": {
									"database_status": true,
									"source_status": true
								}
							}
						}
					}
				]
			`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p1, err := NewProjectMigrate("testdata/projectv3", WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(hgeEndpoint))
			require.NoError(t, err)
			_, err = p1.Apply(tt.args.opts...)
			require.NoError(t, err)

			status, err := p.StatusJSON()
			assert.NoError(t, err)
			statusJsonb, err := ioutil.ReadAll(status)
			assert.NoError(t, err)

			assert.JSONEq(t, tt.want, string(statusJsonb))
		})
	}
}

func TestProjectMigrate_SkipExecution_Configv2(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)

	p, err := NewProjectMigrate("testdata/projectv2", WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(hgeEndpoint))
	require.NoError(t, err)
	_, err = p.Apply(ApplyOnAllDatabases())
	require.NoError(t, err)

	type args struct {
		opts []ProjectMigrationApplierOption
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{
			"mark migration as unapplied",
			args{
				opts: []ProjectMigrationApplierOption{
					ApplyVersion("1623842054907", MigrationDirectionDown), ApplyWithSkipExecution(),
				},
			},
			`
			[
				{
					"databaseName": "default",
					"status": {
						"migrations": [
							1623842054907,
							1623842062104,
							1623842069725,
							1623842076537,
							1623842087940
						],
						"status": {
							"1623842054907": {
								"database_status": false,
								"source_status": true
							},
							"1623842062104": {
								"database_status": true,
								"source_status": true
							},
							"1623842069725": {
								"database_status": true,
								"source_status": true
							},
							"1623842076537": {
								"database_status": true,
								"source_status": true
							},
							"1623842087940": {
								"database_status": true,
								"source_status": true
							}
						}
					}
				}
			]
			`,
		},
		{
			"mark migration as applied",
			args{
				opts: []ProjectMigrationApplierOption{
					ApplyVersion("1623842054907", MigrationDirectionUp), ApplyWithSkipExecution(),
				},
			},
			`
			[
				{
					"databaseName": "default",
					"status": {
						"migrations": [
							1623842054907,
							1623842062104,
							1623842069725,
							1623842076537,
							1623842087940
						],
						"status": {
							"1623842054907": {
								"database_status": true,
								"source_status": true
							},
							"1623842062104": {
								"database_status": true,
								"source_status": true
							},
							"1623842069725": {
								"database_status": true,
								"source_status": true
							},
							"1623842076537": {
								"database_status": true,
								"source_status": true
							},
							"1623842087940": {
								"database_status": true,
								"source_status": true
							}
						}
					}
				}
			]
			`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p1, err := NewProjectMigrate("testdata/projectv2", WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(hgeEndpoint))
			require.NoError(t, err)
			_, err = p1.Apply(tt.args.opts...)
			require.NoError(t, err)

			status, err := p1.StatusJSON()
			assert.NoError(t, err)
			statusJsonb, err := ioutil.ReadAll(status)
			assert.NoError(t, err)

			assert.JSONEq(t, tt.want, string(statusJsonb))
		})
	}
}
