package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"

	"github.com/Pallinder/go-randomdata"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/sources"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	version3 "github.com/hasura/graphql-engine/cli/v2/version"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/require"
	"go.yaml.in/yaml/v3"
)

var _ = Describe("hasura init --endpoint (config v3)", func() {
	var projectDirectory string
	var teardown func()
	var hgeEndpoint string
	sourceName := randomdata.SillyName()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, "default", connectionString)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("should create version 3 metadata directory", func() {
		err := os.RemoveAll(projectDirectory)
		Expect(err).To(BeNil())

		session := testutil.Hasura(testutil.CmdOpts{
			Args: []string{"init", projectDirectory},
		})
		Eventually(session, timeout).Should(Exit(0))

		wantKeywordList := []string{
			fmt.Sprintf("cd %s", projectDirectory),
			"hasura console",
		}
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		// check that required files are present
		wantFilesList := []string{
			filepath.Join(projectDirectory, "AGENTS.md"),
			filepath.Join(projectDirectory, "metadata", "databases", "databases.yaml"),
		}
		for _, file := range wantFilesList {
			_, err := os.ReadFile(file)
			Expect(err).To(BeNil())
		}

		// check contents of metadata/version.yaml
		gotMetadataVersion, err := os.ReadFile(
			filepath.Join(projectDirectory, "metadata", "version.yaml"),
		)
		Expect(err).To(BeNil())
		goldenMetadataVersion, err := os.ReadFile(
			"testdata/init_test/config-v3/metadata/version.golden.yaml",
		)
		Expect(err).To(BeNil())

		Expect(gotMetadataVersion).Should(MatchYAML(goldenMetadataVersion))
	})

	It("should create directory with metadata and migrations from server", func() {
		Context("create migrations and apply those on server", func() {
			testMigrateApply(projectDirectory, []string{"--database-name", sourceName})
		})
		Context("test init --endpoint for config v3", func() {
			err := os.RemoveAll(projectDirectory)
			Expect(err).To(BeNil())
			session := testutil.Hasura(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--endpoint", hgeEndpoint, "--fetch"},
			})

			wantKeywordList := []string{
				fmt.Sprintf("cd %s", projectDirectory),
				"hasura console",
				"Metadata exported",
				"migrations applied",
			}
			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
			fileInfos, err := os.ReadDir(filepath.Join(projectDirectory, "migrations", sourceName))
			Expect(err).To(BeNil())
			Expect(len(fileInfos)).Should(BeEquivalentTo(1))
			upMigrationsContent, err := os.ReadFile(
				filepath.Join(
					projectDirectory,
					"migrations",
					sourceName,
					fileInfos[0].Name(),
					"up.sql",
				),
			)
			Expect(err).To(BeNil())
			Expect(upMigrationsContent).ShouldNot(ContainSubstring("hdb_catalog"))
			Expect(upMigrationsContent).ShouldNot(ContainSubstring("hdb_views"))
		})
	})
})

var _ = Describe("hasura init --endpoint (config v2)", func() {
	var projectDirectory string
	var teardown func()
	var hgeEndpoint string

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		copyTestConfigV2Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("should create version 2 metadata directory", func() {
		err := os.RemoveAll(projectDirectory)
		Expect(err).To(BeNil())

		session := testutil.Hasura(testutil.CmdOpts{
			Args: []string{"init", projectDirectory, "--version", "2"},
		})
		Eventually(session, timeout).Should(Exit(0))

		wantKeywordList := []string{
			fmt.Sprintf("cd %s", projectDirectory),
			"hasura console",
		}
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		// check that required files are present
		wantFilesList := []string{
			filepath.Join(projectDirectory, "metadata", "functions.yaml"),
			filepath.Join(projectDirectory, "metadata", "tables.yaml"),
		}
		for _, file := range wantFilesList {
			_, err := os.ReadFile(file)
			Expect(err).To(BeNil())
		}

		// check contents of metadata/version.yaml
		gotMetadataVersion, err := os.ReadFile(
			filepath.Join(projectDirectory, "metadata", "version.yaml"),
		)
		Expect(err).To(BeNil())
		goldenMetadataVersion, err := os.ReadFile(
			"testdata/init_test/config-v2/metadata/version.golden.yaml",
		)
		Expect(err).To(BeNil())

		Expect(gotMetadataVersion).Should(MatchYAML(goldenMetadataVersion))
	})

	It("should create directory with metadata and migrations from server", func() {
		Context("create migrations and apply those on server", func() {
			testMigrateApply(projectDirectory, nil)
		})
		Context("test init --endpoint for config v2", func() {
			err := os.RemoveAll(projectDirectory)
			Expect(err).To(BeNil())
			session := testutil.Hasura(testutil.CmdOpts{
				Args: []string{
					"init",
					projectDirectory,
					"--endpoint",
					hgeEndpoint,
					"--version",
					"2",
					"--fetch",
				},
			})

			wantKeywordList := []string{
				fmt.Sprintf("cd %s", projectDirectory),
				"hasura console",
				"Metadata exported",
				"migrations applied",
			}
			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
			fileInfos, err := os.ReadDir(filepath.Join(projectDirectory, "migrations"))
			Expect(err).To(BeNil())
			Expect(len(fileInfos)).Should(BeEquivalentTo(1))
			upMigrationsContent, err := os.ReadFile(
				filepath.Join(projectDirectory, "migrations", fileInfos[0].Name(), "up.sql"),
			)
			Expect(err).To(BeNil())
			Expect(string(upMigrationsContent)).ShouldNot(ContainSubstring("hdb_catalog"))
			Expect(string(upMigrationsContent)).ShouldNot(ContainSubstring("hdb_views"))
		})
	})
})

func newInitAgentsOptions(directory string, configVersion cli.ConfigVersion) *InitOptions {
	ec := cli.NewExecutionContext()
	ec.ExecutionDirectory = directory
	ec.Version = version3.New()
	ec.Logger = logrus.New()
	return &InitOptions{EC: ec, Version: configVersion}
}

func TestInitAgentsScaffold(t *testing.T) {
	directory := t.TempDir()
	opts := newInitAgentsOptions(directory, cli.V3)
	// Credentials/endpoint must not leak into the guide.
	opts.Endpoint = "https://private.example.test"
	opts.AdminSecret = "test-secret-not-for-agents"
	require.NoError(t, opts.createFiles())
	filename := filepath.Join(directory, "AGENTS.md")
	data, err := os.ReadFile(filename)
	require.NoError(t, err)
	content := string(data)
	for _, expected := range []string{
		"Hasura GraphQL Engine (v2)",
		"CLI config v3",
		"do not start `hasura console`",
		"metadata/version.yaml",
		"up.sql",
		"down.sql",
		"hasura metadata apply --disallow-inconsistent-metadata",
		"hasura metadata inconsistency status",
		"entire",
		"HASURA_GRAPHQL_ADMIN_SECRET",
		"least privilege",
		"not ordinary",
		"hasura migrate create add_article --database-name <source-name>",
		`tables: "!include default/tables/tables.yaml"`,
		`- "!include public_article.yaml"`,
		"register the source with empty tracking first",
	} {
		require.Contains(t, content, expected)
	}
	require.NotContains(t, content, "CLI config v2")
	require.NotContains(t, content, "{{")
	require.NotContains(t, content, opts.AdminSecret)
	require.NotContains(t, content, opts.Endpoint)
	info, err := os.Stat(filename)
	require.NoError(t, err)
	if runtime.GOOS != "windows" {
		require.Equal(t, os.FileMode(0), info.Mode().Perm()&0o111)
		require.Equal(t, os.FileMode(0), info.Mode().Perm()&0o022)
	}
}

func TestInitAgentsSkippedForV2(t *testing.T) {
	t.Chdir(t.TempDir())
	opts := newInitAgentsOptions("", cli.V2)
	opts.InitDir = "legacy-project"
	require.NoError(t, opts.InitRun())
	require.NoFileExists(t, filepath.Join(opts.InitDir, "AGENTS.md"))
	require.FileExists(t, filepath.Join(opts.InitDir, "config.yaml"))
	require.FileExists(t, filepath.Join(opts.InitDir, "metadata", "tables.yaml"))
}

func TestInitAgentsInitRun(t *testing.T) {
	for _, directory := range []string{"new-project", "."} {
		t.Run(directory, func(t *testing.T) {
			t.Chdir(t.TempDir())
			opts := newInitAgentsOptions("", cli.V3)
			opts.InitDir = directory
			require.NoError(t, opts.InitRun())
			require.FileExists(t, filepath.Join(directory, "AGENTS.md"))
			require.FileExists(t, filepath.Join(directory, "config.yaml"))
			require.FileExists(
				t,
				filepath.Join(directory, "metadata", "databases", "databases.yaml"),
			)
		})
	}
}

func TestInitAgentsPreservesExisting(t *testing.T) {
	for _, content := range []string{"# Project-specific instructions\nKeep these intact.\n", ""} {
		t.Run(strings.TrimSpace(content), func(t *testing.T) {
			directory := t.TempDir()
			t.Chdir(directory)
			filename := filepath.Join(directory, "AGENTS.md")
			require.NoError(t, os.WriteFile(filename, []byte(content), 0o600))
			before, err := os.Stat(filename)
			require.NoError(t, err)
			opts := newInitAgentsOptions("", cli.V3)
			opts.InitDir = "."
			require.NoError(t, opts.InitRun())
			data, err := os.ReadFile(filename)
			require.NoError(t, err)
			require.Equal(t, content, string(data))
			after, err := os.Stat(filename)
			require.NoError(t, err)
			require.Equal(t, before.Mode(), after.Mode())
			require.Equal(t, before.ModTime(), after.ModTime())
		})
	}
}

func TestInitAgentsPreservesSymlinks(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("creating symlinks may require elevated privileges on Windows")
	}
	for _, dangling := range []bool{false, true} {
		t.Run(
			map[bool]string{false: "existing target", true: "dangling target"}[dangling],
			func(t *testing.T) {
				directory := t.TempDir()
				target := filepath.Join(t.TempDir(), "instructions.md")
				if !dangling {
					require.NoError(t, os.WriteFile(target, []byte("custom instructions"), 0o600))
				}
				filename := filepath.Join(directory, "AGENTS.md")
				require.NoError(t, os.Symlink(target, filename))
				require.NoError(t, newInitAgentsOptions(directory, cli.V3).createAgentsFile())
				gotTarget, err := os.Readlink(filename)
				require.NoError(t, err)
				require.Equal(t, target, gotTarget)
				if dangling {
					require.NoFileExists(t, target)
				} else {
					content, err := os.ReadFile(target)
					require.NoError(t, err)
					require.Equal(t, "custom instructions", string(content))
				}
			},
		)
	}
}

func TestInitAgentsExistingDirectory(t *testing.T) {
	directory := t.TempDir()
	require.NoError(t, os.Mkdir(filepath.Join(directory, "AGENTS.md"), 0o755))
	require.NoError(t, newInitAgentsOptions(directory, cli.V3).createFiles())
	require.DirExists(t, filepath.Join(directory, "AGENTS.md"))
}

func TestInitAgentsCreateFailure(t *testing.T) {
	opts := newInitAgentsOptions(filepath.Join(t.TempDir(), "missing"), cli.V3)
	err := opts.createAgentsFile()
	require.ErrorContains(t, err, "cannot create AGENTS.md")
}

// Exercise the guide's YAML snippets through the actual source/include builder,
// so a readable example cannot silently become unusable Hasura metadata.
func TestInitAgentsMetadataExamples(t *testing.T) {
	directory := t.TempDir()
	opts := newInitAgentsOptions(directory, cli.V3)
	require.NoError(t, opts.createFiles())
	data, err := os.ReadFile(filepath.Join(directory, "AGENTS.md"))
	require.NoError(t, err)
	blocks := regexp.MustCompile("(?s)  ```yaml\\n(.*?)  ```").
		FindAllStringSubmatch(string(data), -1)
	for idx := range blocks {
		blocks[idx][1] = strings.TrimPrefix(
			strings.ReplaceAll(blocks[idx][1], "\n  ", "\n"),
			"  ",
		)
	}
	// databases.yaml, tables.yaml, public_article.yaml, then the object and
	// array relationship examples.
	require.Len(t, blocks, 5)
	for _, block := range blocks[3:] {
		var parsed map[string]any
		require.NoError(t, yaml.Unmarshal([]byte(block[1]), &parsed))
	}
	tableDir := filepath.Join(opts.EC.MetadataDir, "databases", "default", "tables")
	require.NoError(t, os.MkdirAll(tableDir, 0o755))
	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(opts.EC.MetadataDir, "databases", "databases.yaml"),
			[]byte(blocks[0][1]),
			0o644,
		),
	)
	require.NoError(
		t,
		os.WriteFile(filepath.Join(tableDir, "tables.yaml"), []byte(blocks[1][1]), 0o644),
	)
	require.NoError(
		t,
		os.WriteFile(
			filepath.Join(tableDir, "public_article.yaml"),
			// The object relationship example extends the article table object.
			[]byte(blocks[2][1]+blocks[3][1]),
			0o644,
		),
	)
	built, err := sources.New(opts.EC, opts.EC.MetadataDir).Build()
	require.NoError(t, err)
	content, err := yaml.Marshal(built)
	require.NoError(t, err)
	var metadata struct {
		Sources []struct {
			Name   string `yaml:"name"`
			Tables []struct {
				Table struct {
					Schema string `yaml:"schema"`
					Name   string `yaml:"name"`
				} `yaml:"table"`
				ObjectRelationships []struct {
					Name string `yaml:"name"`
				} `yaml:"object_relationships"`
				SelectPermissions []struct {
					Role string `yaml:"role"`
				} `yaml:"select_permissions"`
			} `yaml:"tables"`
		} `yaml:"sources"`
	}
	require.NoError(t, yaml.Unmarshal(content, &metadata))
	require.Len(t, metadata.Sources, 1)
	require.Equal(t, "default", metadata.Sources[0].Name)
	require.Len(t, metadata.Sources[0].Tables, 1)
	table := metadata.Sources[0].Tables[0]
	require.Equal(t, "public", table.Table.Schema)
	require.Equal(t, "article", table.Table.Name)
	require.Len(t, table.SelectPermissions, 1)
	require.Equal(t, "user", table.SelectPermissions[0].Role)
	require.Len(t, table.ObjectRelationships, 1)
	require.Equal(t, "author", table.ObjectRelationships[0].Name)
	// The expanded metadata must contain no unresolved CLI include directives.
	require.NotContains(t, string(content), "!include")
}
