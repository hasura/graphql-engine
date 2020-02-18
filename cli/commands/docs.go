package commands

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/assets"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/cobra/doc"
	"github.com/spf13/viper"
)

// NewDocsCmd returns the docs command
func NewDocsCmd(ec *cli.ExecutionContext) *cobra.Command {
	var docType, docDirectory string
	docsCmd := &cobra.Command{
		Use:          "docs",
		Short:        "Generate CLI docs in various formats",
		Hidden:       true,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = viper.New()
			return nil
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
				err = genReSTTreeCustom(rootCmd, docDirectory, "Hasura CLI: ", func(s string) string { return "" }, sphinxLinkHandler)
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

func sphinxLinkHandler(name, ref string) string {
	return fmt.Sprintf(":ref:`%s <%s>`", name, ref)
}

// taken from https://github.com/spf13/cobra/blob/master/doc/rest_docs.go
func printOptionsReST(buf *bytes.Buffer, cmd *cobra.Command, name string) error {
	flags := cmd.NonInheritedFlags()
	flags.SetOutput(buf)
	if flags.HasFlags() {
		buf.WriteString("Options\n")
		buf.WriteString("~~~~~~~\n\n::\n\n")
		flags.PrintDefaults()
		buf.WriteString("\n")
	}

	parentFlags := cmd.InheritedFlags()
	parentFlags.SetOutput(buf)
	if parentFlags.HasFlags() {
		buf.WriteString("Options inherited from parent commands\n")
		buf.WriteString("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n::\n\n")
		parentFlags.PrintDefaults()
		buf.WriteString("\n")
	}
	return nil
}

// linkHandler for default ReST hyperlink markup
func defaultLinkHandler(name, ref string) string {
	return fmt.Sprintf("`%s <%s.rst>`_", name, ref)
}

// genReST creates reStructured Text output.
func genReST(cmd *cobra.Command, w io.Writer, titlePrefix string) error {
	return genReSTCustom(cmd, w, titlePrefix, defaultLinkHandler)
}

// genReSTCustom creates custom reStructured Text output.
func genReSTCustom(cmd *cobra.Command, w io.Writer, titlePrefix string, linkHandler func(string, string) string) error {
	cmd.InitDefaultHelpCmd()
	cmd.InitDefaultHelpFlag()

	buf := new(bytes.Buffer)
	name := cmd.CommandPath()
	ref := strings.Replace(name, " ", "_", -1)
	cliDocPath := "manifests/docs/" + ref + ".rst"
	short := cmd.Short
	long := cmd.Long
	if len(long) == 0 {
		long = short
	}
	fileInfo, er := assets.Asset(cliDocPath)
	var info string
	if er != nil || string(fileInfo) == "" {
		info = short
	} else {
		info = string(fileInfo)
	}

	buf.WriteString(".. _" + ref + ":\n\n")

	buf.WriteString(titlePrefix + name + "\n")
	buf.WriteString(strings.Repeat("-", len(titlePrefix+name)) + "\n\n")
	buf.WriteString(info + "\n\n")

	buf.WriteString("Synopsis\n")
	buf.WriteString("~~~~~~~~\n\n")
	buf.WriteString("\n" + long + "\n\n")

	if cmd.Runnable() {
		buf.WriteString(fmt.Sprintf("::\n\n  %s\n\n", cmd.UseLine()))
	}

	if len(cmd.Aliases) > 0 {
		buf.WriteString("Alias: " + strings.Join(cmd.Aliases, ", ") + "\n\n")
	}

	if len(cmd.Example) > 0 {
		buf.WriteString("Examples\n")
		buf.WriteString("~~~~~~~~\n\n")
		buf.WriteString(fmt.Sprintf("::\n\n%s\n\n", indentString(cmd.Example, "  ")))
	}

	if err := printOptionsReST(buf, cmd, name); err != nil {
		return err
	}
	if hasSeeAlso(cmd) {
		buf.WriteString("SEE ALSO\n")
		buf.WriteString("~~~~~~~~\n\n")
		if cmd.HasParent() {
			parent := cmd.Parent()
			pname := parent.CommandPath()
			ref = strings.Replace(pname, " ", "_", -1)
			buf.WriteString(fmt.Sprintf("* %s \t - %s\n", linkHandler(pname, ref), parent.Short))
			cmd.VisitParents(func(c *cobra.Command) {
				if c.DisableAutoGenTag {
					cmd.DisableAutoGenTag = c.DisableAutoGenTag
				}
			})
		}

		children := cmd.Commands()
		sort.Sort(byName(children))

		for _, child := range children {
			if !child.IsAvailableCommand() || child.IsAdditionalHelpTopicCommand() {
				continue
			}
			cname := name + " " + child.Name()
			ref = strings.Replace(cname, " ", "_", -1)
			buf.WriteString(fmt.Sprintf("* %s \t - %s\n", linkHandler(cname, ref), child.Short))
		}
		buf.WriteString("\n")
	}
	if !cmd.DisableAutoGenTag {
		buf.WriteString("*Auto generated by spf13/cobra*\n")
	}
	_, err := buf.WriteTo(w)
	return err
}

// genReSTTree will generate a ReST page for this command and all
// descendants in the directory given.
// This function may not work correctly if your command names have `-` in them.
// If you have `cmd` with two subcmds, `sub` and `sub-third`,
// and `sub` has a subcommand called `third`, it is undefined which
// help output will be in the file `cmd-sub-third.1`.
func genReSTTree(cmd *cobra.Command, dir, titlePrefix string) error {
	emptyStr := func(s string) string { return "" }
	return genReSTTreeCustom(cmd, dir, titlePrefix, emptyStr, defaultLinkHandler)
}

// genReSTTreeCustom is the the same as genReSTTree, but
// with custom filePrepender and linkHandler.
func genReSTTreeCustom(cmd *cobra.Command, dir, titlePrefix string, filePrepender func(string) string, linkHandler func(string, string) string) error {
	for _, c := range cmd.Commands() {
		if !c.IsAvailableCommand() || c.IsAdditionalHelpTopicCommand() {
			continue
		}
		if err := genReSTTreeCustom(c, dir, titlePrefix, filePrepender, linkHandler); err != nil {
			return err
		}
	}

	basename := strings.Replace(cmd.CommandPath(), " ", "_", -1) + ".rst"
	filename := filepath.Join(dir, basename)
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer f.Close()

	if _, err := io.WriteString(f, filePrepender(filename)); err != nil {
		return err
	}
	if err := genReSTCustom(cmd, f, titlePrefix, linkHandler); err != nil {
		return err
	}
	return nil
}

// adapted from: https://github.com/kr/text/blob/main/indent.go
func indentString(s, p string) string {
	var res []byte
	b := []byte(s)
	prefix := []byte(p)
	bol := true
	for _, c := range b {
		if bol && c != '\n' {
			res = append(res, prefix...)
		}
		res = append(res, c)
		bol = c == '\n'
	}
	return string(res)
}

// Test to see if we have a reason to print See Also information in docs
// Basically this is a test for a parent commend or a subcommand which is
// both not deprecated and not the autogenerated help command.
func hasSeeAlso(cmd *cobra.Command) bool {
	if cmd.HasParent() {
		return true
	}
	for _, c := range cmd.Commands() {
		if !c.IsAvailableCommand() || c.IsAdditionalHelpTopicCommand() {
			continue
		}
		return true
	}
	return false
}

// Temporary workaround for yaml lib generating incorrect yaml with long strings
// that do not contain \n.
func forceMultiLine(s string) string {
	if len(s) > 60 && !strings.Contains(s, "\n") {
		s = s + "\n"
	}
	return s
}

type byName []*cobra.Command

func (s byName) Len() int           { return len(s) }
func (s byName) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s byName) Less(i, j int) bool { return s[i].Name() < s[j].Name() }
