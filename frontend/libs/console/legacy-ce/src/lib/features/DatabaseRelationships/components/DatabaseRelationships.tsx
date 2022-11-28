import { Table } from '@/features/hasura-metadata-types';
import { Button } from '@/new-components/Button';
import { useFireNotification } from '@/new-components/Notifications';
import React, { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';
import { MODE, Relationship } from '../types';
import { AvailableRelationshipsList } from './AvailableRelationshipsList/AvailableRelationshipsList';
import { NOTIFICATIONS } from './constants';
import { RenderWidget } from './RenderWidget/RenderWidget';

export interface DatabaseRelationshipsProps {
  dataSourceName: string;
  table: Table;
}

export const DatabaseRelationships = (props: DatabaseRelationshipsProps) => {
  const { dataSourceName, table } = props;

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

  const onError = (err: Error) => {
    if (mode)
      fireNotification({
        type: 'error',
        title: NOTIFICATIONS.onSuccess[mode],
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
          {...props}
          onAction={(_relationship, _mode) => {
            setTabState({
              mode: _mode,
              relationship: _relationship,
            });
          }}
        />
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
            Create New Relationship
          </Button>
        )}
      </div>
    </div>
  );
};
