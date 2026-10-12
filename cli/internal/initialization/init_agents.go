package initialization

import (
	_ "embed"
	stderrors "errors"
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

//go:embed init_agents.md
var initAgentsGuide string

// createAgentsFile writes config v3 project-local guidance without overwriting
// instructions supplied by the user (including a symlink or directory named AGENTS.md).
func CreateAgentsFile(executionDirectory string) error {
	var op errors.Op = "commands.InitOptions.createAgentsFile"

	filename := filepath.Join(executionDirectory, "AGENTS.md")

	file, err := os.OpenFile(filename, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o644)
	if stderrors.Is(err, os.ErrExist) {
		return nil
	}

	if err != nil {
		return errors.E(op, fmt.Errorf("cannot create AGENTS.md: %w", err))
	}

	_, writeErr := file.WriteString(initAgentsGuide)
	closeErr := file.Close()

	if writeErr != nil {
		// Do not leave partial instructions behind if writing fails.
		_ = os.Remove(filename)

		return errors.E(op, fmt.Errorf("cannot write AGENTS.md: %w", writeErr))
	}

	if closeErr != nil {
		_ = os.Remove(filename)

		return errors.E(op, fmt.Errorf("cannot close AGENTS.md: %w", closeErr))
	}

	return nil
}
