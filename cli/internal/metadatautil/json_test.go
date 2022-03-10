package metadatautil

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestJSONToYAML(t *testing.T) {
	tests := []struct {
		name           string
		inputFile      string
		wantGoldenFile string
		wantErr        bool
	}{
		{
			"can preserve order of json",
			"testdata/json/t1/metadata.json",
			"testdata/json/t1/want.metadata.yaml",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input, err := ioutil.ReadFile(tt.inputFile)
			assert.NoError(t, err)
			got, err := JSONToYAML(input)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				// uncomment to update golden file
				// assert.NoError(t, ioutil.WriteFile(tt.wantGoldenFile, got, os.ModePerm))

				want, err := ioutil.ReadFile(tt.wantGoldenFile)
				assert.NoError(t, err)
				assert.Equal(t, string(want), string(got))
			}

		})
	}
}
