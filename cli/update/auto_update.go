package update

import (
	"io/ioutil"
	"time"

	"github.com/pkg/errors"
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
	err := ioutil.WriteFile(path, []byte(inputTime.Format(timeLayout)), 0644)
	if err != nil {
		return errors.Wrap(err, "failed writing current time to file")
	}
	return nil
}

// ShouldRunCheck checks the file f for a timestamp and returns true
// if the last update check was >= autoCheckInterval .
func ShouldRunCheck(f string) bool {
	lastUpdateTime := getTimeFromFileIfExists(f)
	return time.Since(lastUpdateTime) >= autoCheckInterval
}
