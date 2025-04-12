package errors

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIsKind(t *testing.T) {
	e := E(Op("test"), KindNetwork)
	assert.Equal(t, true, IsKind(Network, e))
}
