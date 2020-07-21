package api

import "github.com/gin-gonic/gin"

type ApplyMigrationRequest struct {
	UpMigration    string `json:"up"`
	DownMigration  string `json:"down"`
	SkipExecution  bool   `json:"skip_execution"`
	MigrateVersion string `json:"version"`
	MigrateType    string `json:"type"`
	GotoVersion    string `json:"goto"`
}

func ApplyMigrationAPI(c *gin.Context) {
	// migratePtr, ok := c.Get("migrate")
	// if !ok {
	// 	return
	// }
	// // Get File url
	// sourcePtr, ok := c.Get("filedir")
	// if !ok {
	// 	return
	// }

}
