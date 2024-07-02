package api

import (
	"errors"
	"fmt"
	"net/http"
	"strconv"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
)

func getMigrationTypeAndStep(upMigration, downMigration, versionMigration, migrationType, gotoVersion string, skipExecution bool) (string, int64, error) {
	var flagCount = 0
	var stepString = "all"
	var migrationName = "up"
	if upMigration != "" {
		stepString = upMigration
		flagCount++
	}
	if downMigration != "" {
		migrationName = "down"
		stepString = downMigration
		flagCount++
	}
	if versionMigration != "" {
		migrationName = "version"
		stepString = versionMigration
		if migrationType == "down" {
			stepString = "-" + stepString
		}
		flagCount++
	}
	if gotoVersion != "" {
		migrationName = "gotoVersion"
		stepString = gotoVersion
		flagCount++
	}

	if flagCount > 1 {
		return "", 0, errors.New("only one migration type can be applied at a time (--up, --down or --goto)")
	}

	if migrationName != "version" && skipExecution {
		return "", 0, errors.New("--skip-execution flag can be set only with --version flag")
	}

	if stepString == "all" && migrationName != "version" {
		return migrationName, -1, nil
	}

	step, err := strconv.ParseInt(stepString, 10, 64)
	if err != nil {
		return "", 0, errors.New("not a valid input for steps/version")
	}
	return migrationName, step, nil
}

func executeMigration(cmd string, t *migrate.Migrate, stepOrVersion int64) error {
	var err error

	switch cmd {
	case "up":
		err = mig.UpCmd(t, stepOrVersion)
	case "down":
		err = mig.DownCmd(t, stepOrVersion)
	case "gotoVersion":
		err = mig.GotoVersionCmd(t, stepOrVersion)
	case "version":
		var direction string
		if stepOrVersion >= 0 {
			direction = "up"
		} else {
			direction = "down"
			stepOrVersion = -(stepOrVersion)
		}
		err = mig.GotoCmd(t, uint64(stepOrVersion), direction)
	default:
		err = fmt.Errorf("Invalid command")
	}

	return err
}

// ApplyMigrationRequest is the request type for the apply API endpoint
type ApplyMigrationRequest struct {
	UpMigration    string `json:"up"`
	DownMigration  string `json:"down"`
	SkipExecution  bool   `json:"skip_execution"`
	MigrateVersion string `json:"version"`
	MigrateType    string `json:"type"`
	GotoVersion    string `json:"goto"`
}

// ApplyMigrationAPI is a wrapper to apply migrations using the API
func ApplyMigrationAPI(c *gin.Context) {
	migratePtr, ok := c.Get("migrate")
	if !ok {
		return
	}

	t := migratePtr.(*migrate.Migrate)

	if c.Request.Method == http.MethodGet {
		var applyMigrationReq ApplyMigrationRequest

		if c.Bind(&applyMigrationReq) != nil {
			c.JSON(http.StatusBadRequest, &Response{Code: "bad_request", Message: "incorrect apply request"})
			return
		}

		upMigration := applyMigrationReq.UpMigration
		downMigration := applyMigrationReq.DownMigration
		migrationVersion := applyMigrationReq.MigrateVersion
		migrationType := applyMigrationReq.MigrateType
		gotoVersion := applyMigrationReq.GotoVersion
		skipExecution := applyMigrationReq.SkipExecution

		migrateType, step, err := getMigrationTypeAndStep(upMigration, downMigration, migrationVersion, migrationType, gotoVersion, skipExecution)

		if err != nil {
			c.JSON(http.StatusBadRequest, &Response{Code: "bad_request", Message: err.Error()})
			return
		}

		t.SkipExecution = skipExecution

		err = executeMigration(migrateType, t, step)

		if err != nil {
			c.JSON(http.StatusBadRequest, &Response{Code: "bad_request", Message: "incorrect apply flags used"})
			return
		}

		c.JSON(http.StatusOK, &Response{Message: "migrations have been applied"})
		return
	}
	c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
}
