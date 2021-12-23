// Copyright 2019 The Kubernetes Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package download

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"hash"
	"io"

	"github.com/pkg/errors"
)

// Verifier can check a reader against it's correctness.
type Verifier interface {
	io.Writer
	Verify() error
}

var _ Verifier = sha256Verifier{}

type sha256Verifier struct {
	hash.Hash
	wantedHash []byte
}

// NewSha256Verifier creates a Verifier that tests against the given hash.
func NewSha256Verifier(hashed string) Verifier {
	raw, _ := hex.DecodeString(hashed)
	return sha256Verifier{
		Hash:       sha256.New(),
		wantedHash: raw,
	}
}

func (v sha256Verifier) Verify() error {
	if bytes.Equal(v.wantedHash, v.Sum(nil)) {
		return nil
	}
	return errors.Errorf("checksum does not match, want: %x, got %x", v.wantedHash, v.Sum(nil))
}
