package errors

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/stretchr/testify/assert"
)

func TestIsKind(t *testing.T) {
	e := errors.E(errors.Op("test"), errors.KindNetwork)
	assert.Equal(t, true, IsKind(Network, e))
}
