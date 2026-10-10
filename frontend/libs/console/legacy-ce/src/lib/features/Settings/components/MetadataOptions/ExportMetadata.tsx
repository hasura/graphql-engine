import { Button } from '@hasura/shared/ui';
import {
  downloadObjectAsJsonFile,
  getCurrTimeForFileName,
} from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification, useMetadata } from '@hasura/metadata/api';

const ExportMetadata = () => {
  const { refetch, isFetching } = useMetadata(undefined, {
    enabled: false,
  });
  const showErrorNotification = useErrorNotification();

  const handleExport = (e) => {
    e.preventDefault();

    refetch()
      .then((data) => {
        const fileName =
          'hasura_metadata_' + getCurrTimeForFileName() + '.json';

        downloadObjectAsJsonFile(fileName, data.data);

        hasuraToast({
          type: 'success',
          title: 'Metadata exported successfully!',
          message: `Metadata file "${fileName}"`,
        });
      })
      .catch((error) => {
        showErrorNotification({
          title: 'Metadata export failed',
          error,
        });
      });
  };

  return (
    <Button
      data-testid="data-export-metadata"
      loading={isFetching}
      loadingText="Exporting..."
      onClick={handleExport}
      mode="default"
    >
      Export metadata
    </Button>
  );
};

export default ExportMetadata;
