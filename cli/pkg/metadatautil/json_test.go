package metadatautil

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var blackhole []byte

func BenchmarkJSONToYAML(b *testing.B) {
	funcs := []struct {
		name string
		f    func([]byte) ([]byte, error)
	}{
		{"cuelang/encoding", JSONToYAML},
	}
	for _, f := range funcs {
		b.Run(f.name, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				input, err := ioutil.ReadFile("testdata/json/t2/metadata.json")
				assert.NoError(b, err)
				blackhole, err = f.f(input)
				assert.NoError(b, err)
			}
		})
	}
}

func TestJSONToYAML(t *testing.T) {
	tests := []struct {
		name           string
		inputFile      string
		wantGoldenFile string
		wantErr        bool
		assertErr      require.ErrorAssertionFunc
	}{
		{
			"can preserve order of json",
			"testdata/json/t1/metadata.json",
			"testdata/json/t1/want.metadata.yaml",
			false,
			require.NoError,
		},
		{
			"can preserve order of json in largish metadata",
			"testdata/json/t2/metadata.json",
			"testdata/json/t2/want.metadata.yaml",
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			input, err := ioutil.ReadFile(tt.inputFile)
			assert.NoError(t, err)
			got, err := JSONToYAML(input)
			tt.assertErr(t, err)
			if !tt.wantErr {
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
