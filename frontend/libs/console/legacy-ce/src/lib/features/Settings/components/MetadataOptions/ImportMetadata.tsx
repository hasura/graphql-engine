import { useState } from 'react';
import { Button } from '@hasura/shared/ui';
import { uploadFile } from '@hasura/shared/utils';
import { useReplaceMetadataFromFile } from '@hasura/metadata/api';

const ImportMetadata = () => {
  const replaceMetadataFromFile = useReplaceMetadataFromFile();
  const [isImporting, setIsImporting] = useState(false);

  const importMetadata = (fileContent) => {
    const successCb = () => {
      setIsImporting(false);
    };

    const errorCb = () => {
      setIsImporting(false);
    };

    setIsImporting(true);

    replaceMetadataFromFile(fileContent, successCb, errorCb);
  };

  const handleImport = (e) => {
    e.preventDefault();

    uploadFile(importMetadata, 'json', null);
  };

  return (
    <div className="inline-block">
      <Button
        data-test="data-import-metadata"
        loading={isImporting}
        loadingText="Importing..."
        onClick={handleImport}
        mode="default"
        size="2"
      >
        Import metadata
      </Button>
    </div>
  );
};

export default ImportMetadata;
