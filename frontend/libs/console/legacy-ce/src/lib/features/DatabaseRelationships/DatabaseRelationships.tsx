import { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../new-components/Button';
import { useFireNotification } from '../../new-components/Notifications';
import { useSyncResourceVersionOnMount } from '../hasura-metadata-api';
import { Table } from '../hasura-metadata-types';
import { AvailableRelationshipsList } from './components/AvailableRelationshipsList/AvailableRelationshipsList';
import Legend from './components/Legend';
import { RenderWidget } from './components/RenderWidget/RenderWidget';
import { SuggestedRelationships } from './components/SuggestedRelationships/SuggestedRelationships';
import { NOTIFICATIONS } from './components/constants';
import { MODE, Relationship } from './types';

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
