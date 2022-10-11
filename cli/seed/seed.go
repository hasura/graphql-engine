package seed

import (
	"io"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

type sendBulk func([]hasura.RequestBody) (io.Reader, error)
type Driver struct {
	SendBulk     sendBulk
	PGDumpClient hasura.PGDump
}

func NewDriver(s sendBulk, pgDumpClient hasura.PGDump) *Driver {
	return &Driver{s, pgDumpClient}
}

func IsSeedsSupported(kind hasura.SourceKind) bool {
	switch kind {
	case hasura.SourceKindMSSQL, hasura.SourceKindPG, hasura.SourceKindCitus:
		return true
	}
	return false
}
