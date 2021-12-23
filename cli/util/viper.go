package util

import (
	"fmt"
	"os"
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
	err := v.BindPFlag(key, f)
	if err != nil {
		fmt.Fprintf(os.Stderr, "viper failed binding pflag: %v with error: %v \n", key, err)
	}
	key = ViperEnvReplacer.Replace(key)
	key = strings.ToUpper(ViperEnvPrefix + "_" + key)
	f.Usage = f.Usage + fmt.Sprintf(` (env "%s")`, key)
}
