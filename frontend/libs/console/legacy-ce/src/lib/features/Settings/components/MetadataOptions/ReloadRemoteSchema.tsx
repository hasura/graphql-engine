import { useState } from 'react';
import { Button } from '@hasura/shared/ui';
import { useReloadRemoteSchema } from '@hasura/metadata/api';

type Props = {
  remoteSchemaName: string;
};

const ReloadRemoteSchema = ({ remoteSchemaName }: Props) => {
  const [isReloading, setIsReloading] = useState(false);
  const reloadRemoteSchema = useReloadRemoteSchema();

  const reloadRemoteMetadataHandler = () => {
    setIsReloading(true);

    reloadRemoteSchema(remoteSchemaName).finally(() => {
      setIsReloading(false);
    });
  };

  return (
    <div className="inline-block">
      <Button
        loading={isReloading}
        data-test="data-reload-metadata"
        onClick={reloadRemoteMetadataHandler}
      >
        Reload
      </Button>
    </div>
  );
};

export default ReloadRemoteSchema;
