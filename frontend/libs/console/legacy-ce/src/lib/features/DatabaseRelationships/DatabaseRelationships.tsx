import { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../new-components/Button';
import { useFireNotification } from '../../new-components/Notifications';
import {
  MetadataSelectors,
  useMetadata,
  useSyncResourceVersionOnMount,
} from '../hasura-metadata-api';
import { Table } from '../hasura-metadata-types';
import { AvailableRelationshipsList } from './components/AvailableRelationshipsList/AvailableRelationshipsList';
import Legend from './components/Legend';
import { RenderWidget } from './components/RenderWidget/RenderWidget';
import { SuggestedRelationships } from './components/SuggestedRelationships/SuggestedRelationships';
import { NOTIFICATIONS } from './components/constants';
import { MODE, Relationship } from './types';
import { useDriverCapabilities } from '../Data/hooks/useDriverCapabilities';
import { Feature } from '../DataSource';
import Skeleton from 'react-loading-skeleton';
import { useAppDispatch } from '../../storeHooks';
import { updateSchemaInfo } from '../../components/Services/Data/DataActions';

export interface DatabaseRelationshipsProps {
  dataSourceName: string;
  table: Table;
}

export const DatabaseRelationships = ({
  dataSourceName,
  table,
}: DatabaseRelationshipsProps) => {
  const [{ mode, relationship }, setTabState] = useState<{
    mode?: MODE;
    relationship?: Relationship;
  }>({
    mode: undefined,
    relationship: undefined,
  });
  const { fireNotification } = useFireNotification();

  const { data: driver } = useMetadata(
    m => MetadataSelectors.findSource(dataSourceName)(m)?.kind
  );
  const dispatch = useAppDispatch();

  const isLoadSchemaRequired = driver === 'mssql' || driver === 'postgres';

  const { data: areForeignKeysSupported, isLoading } = useDriverCapabilities({
    dataSourceName,
    select: data => {
      if (data === Feature.NotImplemented) return false;

      return data.data_schema?.supports_foreign_keys;
    },
  });

  const onCancel = () => {
    setTabState({
      mode: undefined,
      relationship: undefined,
    });
  };

  useSyncResourceVersionOnMount({
    componentName: 'DatabaseRelationships',
  });

  const onError = (err: Error) => {
    if (mode) {
      fireNotification({
        type: 'error',
        title: NOTIFICATIONS.onError[mode],
        message: err?.message ?? '',
      });
      if (isLoadSchemaRequired) {
        dispatch(updateSchemaInfo());
      }
    }
  };

  const onSuccess = () => {
    if (mode) {
      fireNotification({
        type: 'success',
        title: 'Success!',
        message: NOTIFICATIONS.onSuccess[mode],
      });
      if (isLoadSchemaRequired) {
        dispatch(updateSchemaInfo());
      }
    }

    setTabState({
      mode: undefined,
      relationship: undefined,
    });
  };

  if (isLoading) return <Skeleton count={10} height={20} />;

  return (
    <div className="my-2">
      <div>
        <AvailableRelationshipsList
          dataSourceName={dataSourceName}
          table={table}
          onAction={(_relationship, _mode) => {
            setTabState({
              mode: _mode,
              relationship: _relationship,
            });
          }}
        />

        {areForeignKeysSupported && (
          <SuggestedRelationships
            dataSourceName={dataSourceName}
            table={table}
          />
        )}

        <Legend />
      </div>
      <div>
        {mode && (
          <RenderWidget
            dataSourceName={dataSourceName}
            table={table}
            mode={mode}
            relationship={relationship}
            onSuccess={onSuccess}
            onCancel={onCancel}
            onError={onError}
          />
        )}
      </div>
      <div>
        {!mode && (
          <Button
            icon={<FaPlusCircle />}
            onClick={() => {
              setTabState({
                mode: MODE.CREATE,
                relationship: undefined,
              });
            }}
          >
            Add Relationship
          </Button>
        )}
      </div>
    </div>
  );
};
