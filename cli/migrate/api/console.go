package api

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

type MigrationData struct {
	FileContent      string `json:"content"`
	FileType         string `json:"type"`
	MigrationVersion int    `json:"version"`
}

type ConsoleAPIResponse struct {
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
	migrationID := c.Param("migrationID")

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

	if c.Request.Method == http.MethodGet {
		err := t.ReScan()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		status, err := t.GetStatus()

		migrStatus, ok := status.Read(convertToUInt64(migrationID))
		if !ok {
			// TODO: have to change this
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		// logger.Info("Here's some info", migrStatus.Name, "\n")
		migrationName := migrationID + "_" + migrStatus.Name
		upMigrationData, downMigrationData, err := getMigrationInfo(*&sourceURL.Path, migrationName, version)

		if err != nil {
			logger.Error("There has been an error: ", err.Error())
		}

		c.JSON(http.StatusOK, &ConsoleAPIResponse{
			Version:           migrationID,
			UpMigrationData:   upMigrationData,
			DownMigrationData: downMigrationData,
			MigrationStatus:   *migrStatus,
		})
		return
	}

	c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
}
