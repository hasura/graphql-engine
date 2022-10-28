package projectmetadata

import (
	"io"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
)

type PrintContextRichDiffBetweenProjectDirectoriesOpts struct {
	EC                         *cli.ExecutionContext
	MetadataHandler            *Handler
	FromDirectory, ToDirectory string
	Writer                     io.Writer
	DisableColor               bool
}

func PrintContextRichDiffBetweenProjectDirectories(opts PrintContextRichDiffBetweenProjectDirectoriesOpts) error {
	fromObjects := GetMetadataObjectsWithDir(opts.EC, opts.FromDirectory)
	toObjects := GetMetadataObjectsWithDir(opts.EC, opts.ToDirectory)

	toObjectsMap := map[string]metadataobject.Object{}
	for _, object := range toObjects {
		toObjectsMap[object.Key()] = object
	}

	for _, fromObject := range fromObjects {
		diffOpts := metadataobject.WriteDiffOpts{
			To:           toObjectsMap[fromObject.Key()],
			W:            opts.Writer,
			DisableColor: opts.DisableColor,
		}
		err := fromObject.WriteDiff(diffOpts)
		if err != nil {
			opts.EC.Logger.Error(err)
		}
	}
	return nil
}
