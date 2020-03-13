package stub

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/hasura/graphql-engine/cli/migrate/source"
	log "github.com/sirupsen/logrus"
)

func init() {
	source.Register("stub", &Stub{})
}

type Config struct{}

type Stub struct {
	Url        string
	Instance   interface{}
	Migrations *source.Migrations
	Config     *Config
	logger     *log.Logger
}

func (s *Stub) Open(url string, logger *log.Logger) (source.Driver, error) {
	if logger == nil {
		logger = log.New()
	}
	return &Stub{
		Url:        url,
		Migrations: source.NewMigrations(),
		Config:     &Config{},
		logger:     logger,
	}, nil
}

func (s *Stub) Close() error {
	return nil
}

func (s *Stub) Scan() error {
	return nil
}

func (s *Stub) DefaultParser(p source.Parser) {
	return
}

func (s *Stub) First() (version uint64, err error) {
	if v, ok := s.Migrations.First(); !ok {
		return 0, &os.PathError{Op: "first", Path: s.Url, Err: os.ErrNotExist} // TODO: s.Url can be empty when called with WithInstance
	} else {
		return v, nil
	}
}

func (s *Stub) Prev(version uint64) (prevVersion uint64, err error) {
	if v, ok := s.Migrations.Prev(version); !ok {
		return 0, &os.PathError{Op: fmt.Sprintf("prev for version %v", version), Path: s.Url, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (s *Stub) Next(version uint64) (nextVersion uint64, err error) {
	if v, ok := s.Migrations.Next(version); !ok {
		return 0, &os.PathError{Op: fmt.Sprintf("next for version %v", version), Path: s.Url, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (s *Stub) ReadUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := s.Migrations.Up(version); ok {
		return ioutil.NopCloser(bytes.NewBufferString(m.Identifier)), fmt.Sprintf("%v.up.sql.stub", version), fmt.Sprintf("%v.up.sql.stub", version), nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read up sql version %v", version), Path: s.Url, Err: os.ErrNotExist}
}

func (s *Stub) ReadDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := s.Migrations.Down(version); ok {
		return ioutil.NopCloser(bytes.NewBufferString(m.Identifier)), fmt.Sprintf("%v.down.sql.stub", version), fmt.Sprintf("%v.down.sql.stub", version), nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read down sql version %v", version), Path: s.Url, Err: os.ErrNotExist}
}

func (s *Stub) GetDirections(version uint64) map[source.Direction]bool {
	return s.Migrations.GetDirections(version)
}

func (s *Stub) GetLocalVersion() (version uint64, err error) {
	return s.Migrations.GetLocalVersion(), nil
}

func (s *Stub) GetUnappliedMigrations(version uint64) (versions []uint64) {
	return s.Migrations.GetUnappliedMigrations(version)
}

func (s *Stub) ReadMetaUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := s.Migrations.MetaUp(version); ok {
		return ioutil.NopCloser(bytes.NewBufferString(m.Identifier)), fmt.Sprintf("%v.up.yaml.stub", version), fmt.Sprintf("%v.up.yaml.stub", version), nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read up yaml version %v", version), Path: s.Url, Err: os.ErrNotExist}
}

func (s *Stub) ReadMetaDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := s.Migrations.MetaDown(version); ok {
		return ioutil.NopCloser(bytes.NewBufferString(m.Identifier)), fmt.Sprintf("%v.down.yaml.stub", version), fmt.Sprintf("%v.down.yaml.stub", version), nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read down yaml version %v", version), Path: s.Url, Err: os.ErrNotExist}
}

func (f *Stub) ReadName(version uint64) (name string) {
	return f.Migrations.ReadName(version)
}

func (f *Stub) WriteMetadata(files map[string][]byte) error {
	return nil
}
