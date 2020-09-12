package util

import (
	"fmt"
	"strings"

	"github.com/spf13/pflag"
	"github.com/spf13/viper"
)

// ViperEnvPrefix - Env prefix to be used in viper
const ViperEnvPrefix = "HASURA_GRAPHQL"

// ViperEnvReplacer - Env replacer to be used in viper
var ViperEnvReplacer = strings.NewReplacer(".", "_")

// BindPFlag - binds flag with viper along with env usage
func BindPFlag(v *viper.Viper, key string, f *pflag.Flag) {
	v.BindPFlag(key, f)
	key = ViperEnvReplacer.Replace(key)
	key = strings.ToUpper(ViperEnvPrefix + "_" + key)
	f.Usage = f.Usage + fmt.Sprintf(` (env "%s")`, key)
}
