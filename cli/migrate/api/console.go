package api

// NOTE: Should this be covered by tests?

import (
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strconv"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/sirupsen/logrus"
)

const (
	up   = "up"
	down = "down"
)

type UpdateMigrationRequest struct {
	FileName    string `json:"filename"`
	FileContent string `json:"content"`
	// NOTE: we could also keep a log of the changes? would that be necessary?
}

type MigrationData struct {
	FileContent      string `json:"content"`
	FileType         string `json:"type"`
	MigrationVersion int    `json:"version"`
}

type MigrationDataResponse struct {
	Version           string                  `json:"version"`
	UpMigrationData   MigrationData           `json:"up"`
	DownMigrationData MigrationData           `json:"down"`
	MigrationStatus   migrate.MigrationStatus `json:"status"`
}

func convertToUInt64(param string) uint64 {
	num, err := strconv.ParseUint(param, 10, 64)
	if err != nil {
		return 1
	}

	return num
}

// FIXME: May need to change this as well
func getFileType(ext string) string {
	if ext == ".yml" || ext == ".yaml" {
		return "yaml"
	}
	if ext == ".sql" {
		return "sql"
	}

	// TODO: this should also be an error probably
	return ""
}

func getMigrationDirName(version string, name string) string {
	if name == "" {
		return version
	}

	return version + "_" + name
}

func isFilePresent(path string) bool {
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false
	}

	return !info.IsDir()
}

func getMigrationFileContents(path string) (string, error) {
	content, err := ioutil.ReadFile(path)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

func getValidFilePath(path string, migrationType string, extensions []string) (string, string) {
	for _, ext := range extensions {
		fullPath := path + migrationType + ext
		exists := isFilePresent(fullPath)
		if exists {
			return fullPath, ext
		}
	}
	// TODO: this should probably be an error
	return "", ""
}

func getMigrationInfo(path string, migrationName string, version int) (MigrationData, MigrationData, error) {
	// returns the type of file yml, yaml or sql
	// the respective contents of the file
	// this function will only look for up.* and down.* for now

	fullPath := path + "/" + migrationName + "/"
	validExtensions := []string{".yaml", ".sql", ".yml"}

	upSource, upExt := getValidFilePath(fullPath, up, validExtensions)
	downSource, downExt := getValidFilePath(fullPath, down, validExtensions)

	upContent, err := getMigrationFileContents(upSource)
	if err != nil {
		return MigrationData{}, MigrationData{}, err
	}

	downContent, err := getMigrationFileContents(downSource)
	if err != nil {
		return MigrationData{}, MigrationData{}, err
	}

	return MigrationData{
			FileContent:      upContent,
			FileType:         getFileType(upExt),
			MigrationVersion: version,
		}, MigrationData{
			FileContent:      downContent,
			FileType:         getFileType(downExt),
			MigrationVersion: version,
		}, nil
}

func ConsoleAPI(c *gin.Context) {
	migratePtr, ok := c.Get("migrate")
	if !ok {
		return
	}
	// Get File url
	sourcePtr, ok := c.Get("filedir")
	if !ok {
		return
	}

	// Get Logger
	loggerPtr, ok := c.Get("logger")
	if !ok {
		return
	}

	// Get version
	version := c.GetInt("version")

	// Convert to url.URL
	t := migratePtr.(*migrate.Migrate)
	sourceURL := sourcePtr.(*url.URL)
	logger := loggerPtr.(*logrus.Logger)

	migrationVersion := c.Param("migrationVersion")

	// for reading the contents of the
	if c.Request.Method == http.MethodGet {
		err := t.ReScan()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		status, err := t.GetStatus()

		migrStatus, ok := status.Read(convertToUInt64(migrationVersion))
		if !ok {
			// TODO: have to change this code for not a valid migration
			c.JSON(http.StatusBadRequest, &Response{Code: "bad_request", Message: "This is not a valid migration version"})
			return
		}
		// logger.Info("Here's some info", migrStatus.Name, "\n")

		// can be made into a function, also check if migrStatus.Name is not ""
		migrationDirName := getMigrationDirName(migrationVersion, migrStatus.Name)
		upMigrationData, downMigrationData, err := getMigrationInfo(*&sourceURL.Path, migrationDirName, version)

		if err != nil {
			// TODO: send the errorred response too?
			logger.Error("There has been an error: ", err.Error())
		}

		c.JSON(http.StatusOK, &MigrationDataResponse{
			Version:           migrationVersion,
			UpMigrationData:   upMigrationData,
			DownMigrationData: downMigrationData,
			MigrationStatus:   *migrStatus,
		})
		return
	}

	// update the contents of specific migration files
	if c.Request.Method == http.MethodPut {
		err := t.ReScan()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		status, err := t.GetStatus()

		migrStatus, ok := status.Read(convertToUInt64(migrationVersion))
		if !ok {
			// TODO: have to change this
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: ""})
			return
		}

		migrationName := migrStatus.Name

		// NOTE: we perhaps have to check migrStatus and based on the values update the values...
		// or this could just serve as a check if this particular version/timestamp is valid

		var request UpdateMigrationRequest

		if c.Bind(&request) != nil {
			c.JSON(http.StatusBadRequest, &Response{Code: "bad_request", Message: "incorrect update request"})
			return
		}

		fullPath := sourceURL.Path + "/" + getMigrationDirName(migrationVersion, migrationName) + "/" + request.FileName

		err = ioutil.WriteFile(fullPath, []byte(request.FileContent), 0644)

		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "failed to update migration"})
			return
		}

		c.JSON(http.StatusAccepted, &gin.H{"message": "updated migration files", "file": migrationName, "fileContent": request.FileContent})
		return
	}

	// delete the migration files
	if c.Request.Method == http.MethodDelete {
		err := t.ReScan()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		status, err := t.GetStatus()

		migrStatus, ok := status.Read(convertToUInt64(migrationVersion))
		if !ok {
			// TODO: have to change this
			c.JSON(http.StatusBadRequest, &Response{Code: "bad_request", Message: "migration version not found"})
			return
		}

		migrationDir := getMigrationDirName(migrationVersion, migrStatus.Name)
		fullPath := sourceURL.Path + "/" + migrationDir

		if os.RemoveAll(fullPath) != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "migration deletion was unsuccessful"})
			return
		}

		c.JSON(http.StatusAccepted, &gin.H{"message": "deleted migration data", "folder": migrationDir})
		return
	}

	c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
}
