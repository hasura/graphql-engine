package update

import (
	"fmt"
	"io/ioutil"
	"time"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

const (
	autoCheckInterval = 2 * time.Hour
	timeLayout        = time.RFC1123
)

func getTimeFromFileIfExists(path string) time.Time {
	lastUpdateCheckTime, err := ioutil.ReadFile(path)
	if err != nil {
		return time.Time{}
	}
	timeInFile, err := time.Parse(timeLayout, string(lastUpdateCheckTime))
	if err != nil {
		return time.Time{}
	}
	return timeInFile
}

func writeTimeToFile(path string, inputTime time.Time) error {
	var op errors.Op = "update.writeTimeToFile"
	err := ioutil.WriteFile(path, []byte(inputTime.Format(timeLayout)), 0644)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed writing current time to file: %w", err))
	}
	return nil
}

// ShouldRunCheck checks the file f for a timestamp and returns true
// if the last update check was >= autoCheckInterval .
func ShouldRunCheck(f string) bool {
	lastUpdateTime := getTimeFromFileIfExists(f)
	return time.Since(lastUpdateTime) >= autoCheckInterval
}
