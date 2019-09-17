package util

import "github.com/hasura/graphql-engine/cli/version"

const (
	XHasuraAdminSecret = "X-Hasura-Admin-Secret"
	XHasuraAccessKey   = "X-Hasura-Access-Key"
)

func GetAdminSecretHeaderName(v *version.Version) string {
	if v.ServerSemver == nil {
		return XHasuraAdminSecret
	}
	flags, err := v.GetServerFeatureFlags()
	if err != nil {
		return XHasuraAdminSecret
	}
	if flags.HasAccessKey {
		return XHasuraAccessKey
	}
	return XHasuraAdminSecret
}
