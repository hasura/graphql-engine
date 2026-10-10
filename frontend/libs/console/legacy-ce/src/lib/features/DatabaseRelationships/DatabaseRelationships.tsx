import { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';
import { Button, SkeletonList } from '@hasura/shared/ui';
import {
  QualifiedDataSource,
  Table,
  isBulkAtomicResponseError,
} from '@hasura/shared/types';
import { AvailableRelationshipsList } from './components/AvailableRelationshipsList/AvailableRelationshipsList';
import Legend from './components/Legend';
import { RenderWidget } from './components/RenderWidget/RenderWidget';
import { SuggestedRelationships } from './components/SuggestedRelationships/SuggestedRelationships';
import { NOTIFICATIONS } from './components/constants';
import { MODE, Relationship } from './types';
import { hasuraToast } from '@hasura/shared/ui';
import { useDriverCapabilities } from '@hasura/metadata/data-source';
import { useErrorNotification } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';

export interface DatabaseRelationshipsProps {
  source: QualifiedDataSource;
  table: Table;
}

export const DatabaseRelationships = ({
  source,
  table,
}: DatabaseRelationshipsProps) => {
  const showErrorNotification = useErrorNotification();
  const [tabState, setTabState] = useState<{
    mode?: MODE;
    relationship?: Relationship;
  }>({
    mode: undefined,
    relationship: undefined,
  });

  const { data: areForeignKeysSupported, isLoading } = useDriverCapabilities(
    {
      source,
    },
    {
      select: (data) => {
        return data.data_schema?.supports_foreign_keys ?? false;
      },
    },
  );

  const onCancel = () => {
    setTabState({
      mode: undefined,
      relationship: undefined,
    });
  };

  const onError = (err: Error) => {
    if (tabState.mode) {
      showErrorNotification({
        title: NOTIFICATIONS.onError[tabState.mode],
        error: err,
      });
    }
  };

  const onSuccess = (data: unknown) => {
    if (tabState.mode) {
      /**
       * Errors for BulkAtomic are reported with a 500/400 response from the server. We aleady handle this
       * with onError callback
       */
      const errors = Array.isArray(data)
        ? data.filter(isBulkAtomicResponseError)
        : [];

      if (errors.length) {
        showErrorNotification({
          title: NOTIFICATIONS.onError[tabState.mode],
          error: errors,
        });
      } else {
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: NOTIFICATIONS.onSuccess[tabState.mode],
        });
      }
    }

    setTabState({
      mode: undefined,
      relationship: undefined,
    });
  };

  if (isLoading) return <SkeletonList count={5} />;

  return (
    <div className="my-4">
      <Flex direction="column" gap="4">
        <AvailableRelationshipsList
          dataSourceName={source.name}
          table={table}
          onAction={(_relationship, _mode) => {
            setTabState({
              mode: _mode,
              relationship: _relationship,
            });
          }}
        />

        {areForeignKeysSupported && (
          <SuggestedRelationships dataSourceName={source.name} table={table} />
        )}

        <Legend />
      </Flex>
      <div>
        {tabState.mode && (
          <RenderWidget
            dataSourceName={source.name}
            table={table}
            mode={tabState.mode}
            relationship={tabState.relationship}
            onSuccess={onSuccess}
            onCancel={onCancel}
            onError={onError}
          />
        )}
      </div>
      <div>
        {!tabState.mode && (
          <Button
            mode="default"
            type="button"
            leftIcon={FaPlusCircle}
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
