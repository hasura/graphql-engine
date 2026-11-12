import React from 'react';
import { FiSettings } from 'react-icons/fi';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  Checkbox,
  DestructiveDialogCascade,
  getDestructiveDescription,
  hasuraToast,
  showErrorNotification,
  Table,
  useAppearance,
  useCascadeDialog,
} from '@hasura/shared/ui';
import { MetadataTable, QualifiedDataSource } from '@hasura/shared/types';
import { TableDisplayName } from '../components/TableDisplayName';
import {
  TrackableTable,
  useTrackTables,
  useUntrackTables,
} from '@hasura/metadata/api';
import { MongoTrackCollectionModalWrapper } from '../../MongoTrackCollection/MongoTrackCollectionModalWrapper';
import {
  useDriverCapabilities,
  supportsSchemaLessTables,
} from '@hasura/metadata/data-source';
import { dataRoutes } from '@hasura/shared/utils';
import { CustomFieldNames } from '../../CustomFieldNames';

interface TableRowProps {
  source: QualifiedDataSource;
  table: TrackableTable;
  checked: boolean;
  reset: () => void;
  onChange: () => void;
  onTableTrack?: (table: TrackableTable) => void;
  isRowSelectionEnabled: boolean;
}

export const TableRow = React.memo(
  ({
    checked,
    source,
    table,
    reset,
    onChange,
    onTableTrack,
    isRowSelectionEnabled,
  }: TableRowProps) => {
    const { appearance } = useAppearance();
    const [showCustomModal, setShowCustomModal] = React.useState(false);
    const [isMongoTrackingModalVisible, setShowMongoTrackingModalVisible] =
      React.useState(false);
    const { trackTables, isPending: trackLoading } = useTrackTables();
    const { mutate: untrackTables, isPending: untrackLoading } =
      useUntrackTables();
    const untrackDialog = useCascadeDialog();

    const { data: capabilities } = useDriverCapabilities({
      source,
    });

    const areSchemaLessTablesSupported = supportsSchemaLessTables(capabilities);

    const track = (customConfiguration?: MetadataTable['configuration']) => {
      const t = { ...table };
      if (customConfiguration) {
        t.configuration = customConfiguration;
      }

      trackTables(
        {
          tables: [t],
          source: source.name,
        },
        {
          onSuccess: () => {
            reset();
            setShowCustomModal(false);
            onTableTrack?.(table);
          },
        },
      );
    };

    const untrack = (cascade: boolean) => {
      untrackDialog.clearError();
      untrackTables(
        {
          tables: [{ table: table.table, cascade }],
          source: source.name,
        },
        {
          onSuccess: () => {
            untrackDialog.close();
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Object untracked successfully.',
            });
            reset();
            onTableTrack?.(table);
          },
          onError: (err) =>
            untrackDialog.handleError(err, (error) =>
              showErrorNotification({
                title: 'Unable to perform operation',
                error,
              }),
            ),
        },
      );
    };

    return (
      <Table.Row
        className={
          checked
            ? appearance === 'dark'
              ? 'bg-gray-800'
              : 'bg-indigo-50'
            : 'bg-transparent'
        }
      >
        {isRowSelectionEnabled && (
          <Table.Cell className="w-10">
            <Flex align="center" className="h-full">
              <Checkbox value={checked} onChange={onChange} />
            </Flex>
          </Table.Cell>
        )}
        <Table.Cell>
          <TableDisplayName
            to={
              table.is_tracked
                ? dataRoutes.manageTable(source.name, table.table)
                : undefined
            }
            table={table.table}
          />
        </Table.Cell>
        <Table.Cell>
          <Flex align="center" className="h-full">
            {table.type}
          </Flex>
        </Table.Cell>
        <Table.Cell>
          <Flex direction="row" align="center" gap="2" className="h-full">
            {table.is_tracked ? (
              <Button
                data-testid={`untrack-${table.name}`}
                size="sm"
                onClick={() => untrackDialog.open(true)}
                loading={untrackLoading}
                loadingText="Please wait"
              >
                Untrack
              </Button>
            ) : (
              <>
                <Button
                  mode="primary"
                  data-testid={`track-${table.name}`}
                  size="sm"
                  onClick={() => {
                    if (areSchemaLessTablesSupported) {
                      setShowMongoTrackingModalVisible(true);
                      return;
                    }
                    track();
                  }}
                  loading={trackLoading}
                  loadingText="Please wait"
                >
                  Track
                </Button>
                {!areSchemaLessTablesSupported &&
                  !trackLoading &&
                  !untrackLoading && (
                    <Button
                      mode="default"
                      size="sm"
                      onClick={() => {
                        if (areSchemaLessTablesSupported) {
                          setShowMongoTrackingModalVisible(true);
                          return;
                        }
                        setShowCustomModal(true);
                      }}
                      leftIcon={FiSettings}
                    >
                      Customize &amp; Track
                    </Button>
                  )}

                <CustomFieldNames.Modal
                  tableName={table.name}
                  onSubmit={(formValues, config) => {
                    track(config);
                  }}
                  onClose={() => {
                    setShowCustomModal(false);
                  }}
                  callToAction="Customize & Track"
                  callToDeny="Cancel"
                  callToActionLoadingText="Saving..."
                  isLoading={trackLoading}
                  show={showCustomModal}
                  source={source.name}
                />

                {isMongoTrackingModalVisible && (
                  <MongoTrackCollectionModalWrapper
                    dataSourceName={source.name}
                    collectionName={table.name}
                    isVisible={isMongoTrackingModalVisible}
                    onClose={() => setShowMongoTrackingModalVisible(false)}
                  />
                )}
              </>
            )}
          </Flex>
          {untrackDialog.isOpen && (
            <DestructiveDialogCascade
              {...untrackDialog.dialogProps}
              title="Untrack table"
              confirmButtonProps={{
                children: 'Untrack',
                loadingText: 'Untracking...',
              }}
              onConfirm={untrack}
              loading={untrackLoading}
            >
              {getDestructiveDescription({
                destroyTerm: 'untrack',
                resourceName: table.name,
                resourceType: 'table',
              })}
            </DestructiveDialogCascade>
          )}
        </Table.Cell>
      </Table.Row>
    );
  },
);
