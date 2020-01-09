package installation

import (
	"fmt"
	"runtime"

	"github.com/hasura/graphql-engine/cli/plugins/types"
)

// GetMatchingPlatform finds the platform spec in the specified plugin that
// matches the os/arch of the current machine
func GetMatchingPlatform(platforms []types.Platform) (types.Platform, bool, error) {
	return matchPlatform(platforms)
}

// matchPlatform returns the first matching platform to given os/arch.
func matchPlatform(platforms []types.Platform) (types.Platform, bool, error) {
	currSelector := fmt.Sprintf("%s-%s", runtime.GOOS, runtime.GOARCH)

	for _, platform := range platforms {
		if platform.Selector == currSelector {
			return platform, true, nil
		}
	}
	return types.Platform{}, false, nil
}
