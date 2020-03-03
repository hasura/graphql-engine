package file

import (
	"fmt"
	"io"
	"io/ioutil"
	nurl "net/url"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/hasura/graphql-engine/cli/migrate/source"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

type File struct {
	url           string
	path          string
	defaultParser source.Parser
	Migrations    *source.Migrations
	logger        *log.Logger
}

func init() {
	source.Register("file", &File{})
}

func New(url string, logger *log.Logger) (*File, error) {
	if logger == nil {
		logger = log.New()
	}
	u, err := nurl.Parse(url)
	if err != nil {
		return nil, err
	}

	// concat host and path to restore full path
	// host might be `.`
	p := u.Host + u.Path

	if len(p) == 0 {
		// default to current directory if no path
		wd, err := os.Getwd()
		if err != nil {
			return nil, err
		}
		p = wd
	} else if p[0:1] == "." || p[0:1] != "/" {
		// make path absolute if relative
		abs, err := filepath.Abs(p)
		if err != nil {
			return nil, err
		}
		p = abs
	}

	if runtime.GOOS == "windows" && p[0:1] == "/" {
		p = strings.TrimPrefix(p, "/")
	}

	nf := &File{
		url:           url,
		logger:        logger,
		path:          p,
		defaultParser: source.DefaultParse,
		Migrations:    source.NewMigrations(),
	}
	return nf, nil
}

func (f *File) Open(url string, logger *log.Logger) (source.Driver, error) {
	return New(url, logger)
}

func (f *File) Close() error {
	// nothing do to here
	return nil
}

func (f *File) DefaultParser(p source.Parser) {
	f.defaultParser = p
}

func (f *File) Scan() error {
	f.Migrations = source.NewMigrations()
	folders, err := ioutil.ReadDir(f.path)
	if err != nil {
		return err
	}

	for _, fo := range folders {
		if fo.IsDir() {
			// v2 migrate
			dirName := fo.Name()
			dirPath := filepath.Join(f.path, dirName)
			files, err := ioutil.ReadDir(dirPath)
			if err != nil {
				return err
			}
			for _, fi := range files {
				if fi.IsDir() {
					continue
				}
				fileName := fmt.Sprintf("%s.%s", dirName, fi.Name())
				m, err := f.defaultParser(fileName)
				if err != nil {
					continue // ignore files that we can't parse
				}
				m.Raw = filepath.Join(dirName, fi.Name())
				m.IsDir = true
				ok, err := source.IsEmptyFile(m, f.path)
				if err != nil {
					return err
				}
				if !ok {
					continue
				}
				err = f.Migrations.Append(m)
				if err != nil {
					return err
				}
			}
		} else {
			// v1 migrate
			m, err := f.defaultParser(fo.Name())
			if err != nil {
				continue // ignore files that we can't parse
			}
			m.Raw = fo.Name()
			ok, err := source.IsEmptyFile(m, f.path)
			if err != nil {
				return err
			}
			if !ok {
				continue
			}
			err = f.Migrations.Append(m)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (f *File) First() (version uint64, err error) {
	if v, ok := f.Migrations.First(); !ok {
		return 0, &os.PathError{Op: "first", Path: f.path, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (f *File) GetLocalVersion() (version uint64, err error) {
	return f.Migrations.GetLocalVersion(), nil
}

func (f *File) GetUnappliedMigrations(version uint64) (versions []uint64) {
	return f.Migrations.GetUnappliedMigrations(version)
}

func (f *File) Prev(version uint64) (prevVersion uint64, err error) {
	if v, ok := f.Migrations.Prev(version); !ok {
		return 0, &os.PathError{Op: fmt.Sprintf("prev for version %v", version), Path: f.path, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (f *File) Next(version uint64) (nextVersion uint64, err error) {
	if v, ok := f.Migrations.Next(version); !ok {
		return 0, &os.PathError{Op: fmt.Sprintf("next for version %v", version), Path: f.path, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (f *File) GetDirections(version uint64) map[source.Direction]bool {
	return f.Migrations.GetDirections(version)
}

func (f *File) ReadUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.Migrations.Up(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadMetaUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.Migrations.MetaUp(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.Migrations.Down(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadMetaDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.Migrations.MetaDown(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadName(version uint64) (name string) {
	return f.Migrations.ReadName(version)
}

func (f *File) WriteMetadata(files map[string][]byte) error {
	for name, content := range files {
		err := ioutil.WriteFile(name, content, 0644)
		if err != nil {
			return errors.Wrapf(err, "creating metadata file %s failed", name)
		}
	}
	return nil
}
