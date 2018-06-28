package util

import (
	"io"
	"net/http"
	"os"
)

// Download downloads resource given by url and writes it to target.
// target should not exist already, as it is created by the function.
func Download(url, target string) error {
	out, err := os.Create(target)
	if err != nil {
		return err
	}
	defer out.Close()
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	_, err = io.Copy(out, resp.Body)
	if err != nil {
		return err
	}
	return nil
}
