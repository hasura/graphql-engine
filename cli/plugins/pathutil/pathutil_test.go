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

package pathutil

import (
	"path/filepath"
	"reflect"
	"testing"
)

func TestIsSubPathExtending(t *testing.T) {
	type args struct {
		basePath      string
		extendingPath string
	}
	tests := []struct {
		name            string
		args            args
		wantExtending   string
		wantIsExtending bool
	}{
		{
			name: "is extending",
			args: args{
				basePath:      filepath.FromSlash("a/b"),
				extendingPath: filepath.FromSlash("a/b/c"),
			},
			wantExtending:   "c",
			wantIsExtending: true,
		},
		{
			name: "is extending same length",
			args: args{
				basePath:      filepath.FromSlash("a/b/c"),
				extendingPath: filepath.FromSlash("a/b/c"),
			},
			wantExtending:   ".",
			wantIsExtending: true,
		},
		{
			name: "is not extending",
			args: args{
				basePath:      filepath.FromSlash("a/b"),
				extendingPath: filepath.FromSlash("a/a/c"),
			},
		},
		{
			name: "is not smaller extending",
			args: args{
				basePath:      filepath.FromSlash("a/b"),
				extendingPath: filepath.FromSlash("a"),
			},
		},
		{
			name: "base path is not clean",
			args: args{
				basePath:      filepath.FromSlash("d/../a/b/../../a"),
				extendingPath: filepath.FromSlash("a/b"),
			},
			wantExtending:   "b",
			wantIsExtending: true,
		},
		{
			name: "extending path is not clean",
			args: args{
				basePath:      filepath.FromSlash("a"),
				extendingPath: filepath.FromSlash("d/../a/b/c/.."),
			},
			wantExtending:   "b",
			wantIsExtending: true,
		},
		{
			name: "base path is absolute",
			args: args{
				basePath:      filepath.FromSlash("/a"),
				extendingPath: filepath.FromSlash("a/b"),
			},
		},
		{
			name: "extending path is absolute",
			args: args{
				basePath:      filepath.FromSlash("a"),
				extendingPath: filepath.FromSlash("/a/b"),
			},
		},
		{
			name: "both paths are absolute",
			args: args{
				basePath:      filepath.FromSlash("/a"),
				extendingPath: filepath.FromSlash("/a/b"),
			},
			wantExtending:   "b",
			wantIsExtending: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotExtending, gotIsExtending := IsSubPath(tt.args.basePath, tt.args.extendingPath)
			if !reflect.DeepEqual(gotExtending, tt.wantExtending) {
				t.Errorf("IsSubPath() gotExtending = %v, want %v", gotExtending, tt.wantExtending)
			}
			if gotIsExtending != tt.wantIsExtending {
				t.Errorf("IsSubPath() gotIsExtending = %v, want %v", gotIsExtending, tt.wantIsExtending)
			}
		})
	}
}

func TestReplaceBase(t *testing.T) {
	type args struct {
		path        string
		old         string
		replacement string
	}
	tests := []struct {
		name    string
		args    args
		want    string
		wantErr bool
	}{
		{
			name: "test replace",
			args: args{
				path:        filepath.FromSlash("a/b/c"),
				old:         filepath.FromSlash("a"),
				replacement: filepath.FromSlash("d"),
			},
			want:    filepath.FromSlash("d/b/c"),
			wantErr: false,
		},
		{
			name: "test can't replace",
			args: args{
				path:        filepath.FromSlash("a/b/c"),
				old:         filepath.FromSlash("z"),
				replacement: filepath.FromSlash("d"),
			},
			want:    "",
			wantErr: true,
		},
		{
			name: "oldPath isn't matching with substring check",
			args: args{
				path:        filepath.FromSlash("a/bbb"),
				old:         filepath.FromSlash("a/b"),
				replacement: filepath.FromSlash("d"),
			},
			want:    "",
			wantErr: true,
		},
		{
			name: "multiple segments replace",
			args: args{
				path:        filepath.FromSlash("c/c/n/a"),
				old:         filepath.FromSlash("c/c"),
				replacement: filepath.FromSlash("b/a/n/a"),
			},
			want:    filepath.FromSlash("b/a/n/a/n/a"),
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ReplaceBase(tt.args.path, tt.args.old, tt.args.replacement)
			if (err != nil) != tt.wantErr {
				t.Errorf("ReplaceBase() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("ReplaceBase() = %v, want %v", got, tt.want)
			}
		})
	}
}
