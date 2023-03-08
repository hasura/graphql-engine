import { useState, useEffect } from 'react';
import { Table } from '../hasura-metadata-types';
import { Button } from '../../new-components/Button';
import { useFireNotification } from '../../new-components/Notifications';
import { FaPlusCircle } from 'react-icons/fa';
import Legend from './components/Legend';
import { SuggestedRelationships } from './components/SuggestedRelationships/SuggestedRelationships';
import { MODE, Relationship } from './types';
import { AvailableRelationshipsList } from './components/AvailableRelationshipsList/AvailableRelationshipsList';
import { NOTIFICATIONS } from './components/constants';
import { RenderWidget } from './components/RenderWidget/RenderWidget';
import { useInvalidateMetadata } from '../hasura-metadata-api';

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

  const invalidateMetadata = useInvalidateMetadata();

  const onCancel = () => {
    setTabState({
      mode: undefined,
      relationship: undefined,
    });
  };

  // just invalidate metadata when this screen loads for the first time
  // why? because the user might be coming from a redux based paged and the resource_version might gone out of sync
  useEffect(() => {
    invalidateMetadata();
  }, [invalidateMetadata]);

  const onError = (err: Error) => {
    if (mode)
      fireNotification({
        type: 'error',
        title: NOTIFICATIONS.onError[mode],
        message: err?.message ?? '',
      });
  };

  const onSuccess = () => {
    if (mode)
      fireNotification({
        type: 'success',
        title: 'Success!',
        message: NOTIFICATIONS.onSuccess[mode],
      });

    setTabState({
      mode: undefined,
      relationship: undefined,
    });
  };

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

        <SuggestedRelationships dataSourceName={dataSourceName} table={table} />

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
