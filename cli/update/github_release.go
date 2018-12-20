package update

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"time"
)

// Asset structure from GitHub API
type Asset struct {
	ID                 int       `json:"id"`
	URL                string    `json:"url"`
	Name               string    `json:"name"`
	Size               int       `json:"size"`
	CreatedAt          time.Time `json:"created_at"`
	UpdatedAt          time.Time `json:"updated_at"`
	BrowserDownloadURL string    `json:"browser_download_url"`
}

func (a *Asset) DownloadBinary(tmpFile string, path string) (file *os.File, err error) {
	response, err := http.Get(a.BrowserDownloadURL)
	if err != nil {
		return nil, err
	}
	defer response.Body.Close()

	file, err = ioutil.TempFile(path, tmpFile)
	if err != nil {
		return
	}
	defer file.Close()

	_, err = io.Copy(file, response.Body)
	if err != nil {
		return nil, err
	}

	err = file.Chmod(0755)
	if err != nil {
		return nil, err
	}

	return file, nil
}

// Release structure from GitHub API
type Release struct {
	ID          int       `json:"id"`
	URL         string    `json:"url"`
	AssetURL    string    `json:"asset_url"`
	UploadURL   string    `json:"upload_url"`
	HTMLURL     string    `json:"html_url"`
	TagName     string    `json:"tag_name"`
	Name        string    `json:"name"`
	CreatedAt   time.Time `json:"created_at"`
	PublishedAt time.Time `json:"published_at"`
	Prerelease  bool      `json:"prerelease"`
	Assets      []Asset   `json:"assets"`
}

func (r *Release) GetAsset(fileName string) (assetToDownload *Asset) {
	for _, asset := range r.Assets {
		if asset.Name == fileName {
			assetToDownload = &asset
			break
		}
	}
	return
}

const githubReleaseURL string = "https://api.github.com/repos/%s/%s/releases"

func getLatestRelease(repoOwner string, repoName string) (releaseInfo []*Release, err error) {
	resp, err := http.Get(fmt.Sprintf(githubReleaseURL, repoOwner, repoName))
	if err != nil {
		return
	}

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return
	}

	err = json.Unmarshal(body, &releaseInfo)
	if err != nil {
		return
	}
	return
}
