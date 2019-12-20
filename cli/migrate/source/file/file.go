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
	log "github.com/sirupsen/logrus"
)

type File struct {
	url        string
	path       string
	migrations *source.Migrations
	logger     *log.Logger
}

func init() {
	source.Register("file", &File{})
}

func (f *File) Open(url string, logger *log.Logger) (source.Driver, error) {
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
		url:        url,
		logger:     logger,
		path:       p,
		migrations: source.NewMigrations(),
	}

	err = nf.Scan()
	if err != nil {
		return nil, err
	}
	return nf, nil
}

func (f *File) Close() error {
	// nothing do to here
	return nil
}

func (f *File) Scan() error {
	f.migrations = source.NewMigrations()
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
				m, err := source.DefaultParse(fileName)
				if err != nil {
					continue // ignore files that we can't parse
				}
				m.Raw = filepath.Join(dirName, fi.Name())
				ok, err := source.IsEmptyFile(m, f.path)
				if err != nil {
					return err
				}
				if !ok {
					continue
				}
				err = f.migrations.Append(m)
				if err != nil {
					return err
				}
			}
		} else {
			// v1 migrate
			m, err := source.DefaultParse(fo.Name())
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
			err = f.migrations.Append(m)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (f *File) First() (version uint64, err error) {
	if v, ok := f.migrations.First(); !ok {
		return 0, &os.PathError{Op: "first", Path: f.path, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (f *File) GetLocalVersion() (version uint64, err error) {
	return f.migrations.GetLocalVersion(), nil
}

func (f *File) GetUnappliedMigrations(version uint64) (versions []uint64) {
	return f.migrations.GetUnappliedMigrations(version)
}

func (f *File) Prev(version uint64) (prevVersion uint64, err error) {
	if v, ok := f.migrations.Prev(version); !ok {
		return 0, &os.PathError{Op: fmt.Sprintf("prev for version %v", version), Path: f.path, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (f *File) Next(version uint64) (nextVersion uint64, err error) {
	if v, ok := f.migrations.Next(version); !ok {
		return 0, &os.PathError{Op: fmt.Sprintf("next for version %v", version), Path: f.path, Err: os.ErrNotExist}
	} else {
		return v, nil
	}
}

func (f *File) GetDirections(version uint64) map[source.Direction]bool {
	return f.migrations.GetDirections(version)
}

func (f *File) ReadUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.migrations.Up(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadMetaUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.migrations.MetaUp(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.migrations.Down(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadMetaDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error) {
	if m, ok := f.migrations.MetaDown(version); ok {
		r, err := os.Open(path.Join(f.path, m.Raw))
		if err != nil {
			return nil, "", "", err
		}
		return r, m.Identifier, m.Raw, nil
	}
	return nil, "", "", &os.PathError{Op: fmt.Sprintf("read version %v", version), Path: f.path, Err: os.ErrNotExist}
}

func (f *File) ReadName(version uint64) (name string) {
	return f.migrations.ReadName(version)
}
