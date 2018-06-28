package commands

import (
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/cobra/doc"
)

func NewDocsCmd(ec *cli.ExecutionContext) *cobra.Command {
	var docType, docDirectory string
	docsCmd := &cobra.Command{
		Use:          "docs",
		Short:        "Generate CLI docs in various formats",
		Hidden:       true,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) (err error) {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) (err error) {
			err = os.MkdirAll(docDirectory, os.ModePerm)
			if err != nil {
				return errors.Wrap(err, "unable to create directory")
			}
			switch docType {
			case "man":
				err = doc.GenManTree(rootCmd, &doc.GenManHeader{Title: "HASURA", Section: "3"}, docDirectory)
			case "md":
				err = doc.GenMarkdownTree(rootCmd, docDirectory)
			case "rest":
				err = doc.GenReSTTree(rootCmd, docDirectory)
			case "yaml":
				err = doc.GenYamlTree(rootCmd, docDirectory)
			default:
				return errors.New("unknown type")
			}
			if err != nil {
				return errors.Wrap(err, "generating docs failed")
			}
			ec.Logger.Infof("[%s] docs generated in [%s]", docType, docDirectory)
			return nil
		},
	}

	f := docsCmd.Flags()
	f.StringVar(&docType, "type", "md", "type of documentation to generate (man, md, rest, yaml)")
	f.StringVar(&docDirectory, "directory", "docs", "directory where docs should be generated")
	return docsCmd
}
