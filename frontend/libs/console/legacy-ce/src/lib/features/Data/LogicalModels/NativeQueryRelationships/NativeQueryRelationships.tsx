import { AiOutlineReload } from 'react-icons/ai';
import { Button } from '../../../../new-components/Button';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { hasuraToast } from '../../../../new-components/Toasts';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';
import { useTrackNativeQueryRelationships } from '../../hooks/useTrackNativeQueryRelationships/useTrackNativeQueryRelationships';
import { ListNativeQueryRelationships } from './components';
import { useWidget } from './hooks/useWidget';
import Skeleton from 'react-loading-skeleton';
import { ListNativeQueryRow } from './components/ListNativeQueryRelationships';
import { useDestructiveAlert } from '../../../../new-components/Alert';
import { useCallback } from 'react';

export type NativeQueryRelationshipProps = {
  dataSourceName: string;
  nativeQueryName: string;
};

export const NativeQueryRelationships = (
  props: NativeQueryRelationshipProps
) => {
  const { dataSourceName, nativeQueryName } = props;

  const { untrackNativeQueryRelationship, trackNativeQueryRelationship } =
    useTrackNativeQueryRelationships(dataSourceName, nativeQueryName);

  const {
    data: sourceNativeQuery,
    isLoading: isMetadataLoading,
    error: sourceNativeQueryNotFoundError,
    refetch,
    isRefetching,
  } = useMetadata(m =>
    MetadataSelectors.findNativeQuery(dataSourceName, nativeQueryName)(m)
  );

  const handleError = useCallback((err: Error) => {
    const error: string = err.message;
    try {
      const parsed = JSON.parse(error);
      if (Array.isArray(parsed) && Array.isArray(parsed[parsed.length - 1])) {
        const lastItem = parsed[parsed.length - 1];
        if (Array.isArray(lastItem)) {
          const listOfReasons: string[] = [];
          lastItem.forEach(err => {
            if ('reason' in err) {
              listOfReasons.push(err.reason);
            }
          });
          hasuraToast({
            type: 'error',
            title: 'Failed to edit relationship',
            children: (
              <DisplayToastErrorMessage message={listOfReasons.join('\n')} />
            ),
          });
        }
      } else {
        throw new Error(
          'Error message is not an array, falling back to showing error.message in raw string format'
        );
      }
    } catch {
      hasuraToast({
        type: 'error',
        title: 'Failed to edit relationship',
        children: <DisplayToastErrorMessage message={err.message} />,
      });
    }
  }, []);

  const { WidgetUI, openCreate, openEdit, closeWidget } = useWidget({
    nativeQueryName,
    dataSourceName,
    onSubmit: params => {
      const { values, mode } = params;
      trackNativeQueryRelationship({
        data: {
          name: values.name,
          type: values.type,
          using: {
            column_mapping: values.columnMapping,
            remote_native_query: values.toNativeQuery,
            insertion_order: null,
          },
        },
        editDetails:
          params.mode === 'edit'
            ? {
                name: params.originalRelationshipName,
                type: params.originalRelationshipType,
              }
            : undefined,

        onSuccess: () => {
          closeWidget();
          hasuraToast({
            type: 'success',
            title: `Successfully ${
              mode === 'create' ? 'created' : 'edited'
            } relationship "${values.name}"`,
          });
        },
        onError: handleError,
      });
    },
  });

  const { destructiveConfirm } = useDestructiveAlert();
  const handleDelete = (data: ListNativeQueryRow) => {
    destructiveConfirm({
      resourceName: data.name,
      resourceType: 'relationship',
      destroyTerm: 'remove',
      onConfirm: () =>
        new Promise(resolve => {
          untrackNativeQueryRelationship({
            data: {
              name: data.name,
              type: data.type,
            },
            onSuccess: () => {
              resolve(true);
            },
            onError: err => {
              resolve(false);
              hasuraToast({
                type: 'error',
                title: 'Failed to delete relationship',
                children: <DisplayToastErrorMessage message={err.message} />,
              });
            },
          });
        }),
    });
  };

  const handleEdit = (data: ListNativeQueryRow) => {
    openEdit({
      name: data.name,
      columnMapping: data.using.column_mapping,
      type: data.type,
      toNativeQuery: data.using.remote_native_query,
    });
  };

  if (isMetadataLoading) return <Skeleton count={10} />;

  if (sourceNativeQueryNotFoundError)
    return (
      <div>
        <IndicatorCard status="negative">
          {sourceNativeQueryNotFoundError.message ??
            JSON.stringify(sourceNativeQueryNotFoundError)}
        </IndicatorCard>
      </div>
    );

  if (!sourceNativeQuery)
    return (
      <div>
        <IndicatorCard status="info">
          Could not find Native Query : {nativeQueryName} in metadata. Please
          reload metadata and try again.
          <Button
            icon={<AiOutlineReload />}
            onClick={() => refetch()}
            isLoading={isRefetching}
          >
            Reload Metadata
          </Button>
        </IndicatorCard>
      </div>
    );

  return (
    <div>
      <div className="h-4" />
      <ListNativeQueryRelationships
        dataSourceName={dataSourceName}
        nativeQueryName={nativeQueryName}
        onEditRow={handleEdit}
        onDeleteRow={handleDelete}
      />
      <Button mode="primary" onClick={() => openCreate()}>
        Add Relationship
      </Button>
      {WidgetUI()}
    </div>
  );
};
