package errors

import (
	"errors"
	"fmt"
	"net/url"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestUnwrappingErrorsWithKind(t *testing.T) {
	e := E("Test", KindNetwork, &url.Error{
		Op:  "Test URL Error",
		URL: "https://test-url",
		Err: fmt.Errorf("something"),
	})

	assert.True(t, IsKind(KindNetwork, e))
	var urlErr *url.Error
	assert.True(t, errors.As(e, &urlErr))
}

func TestCanGetWrappedKind(t *testing.T) {
	e1 := E("E1", KindHasuraAPI, "some bad error")
	e2 := E("E2", fmt.Errorf("something bad occured: %w", e1))
	assert.True(t, IsKind(KindHasuraAPI, e2))
}
