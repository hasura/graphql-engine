import React from 'react';
import { FiSettings } from 'react-icons/fi';
import { CustomFieldNames } from '../..';
import { APIError } from '../../../../hooks/error';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { hasuraToast } from '../../../../new-components/Toasts';
import { MetadataTable } from '../../../hasura-metadata-types';
import { useTrackTables } from '../../hooks/useTrackTables';
import { TableDisplayName } from '../components/TableDisplayName';
import { TrackableTable } from '../types';
import { MongoTrackCollectionModalWrapper } from '../../MongoTrackCollection/MongoTrackCollectionModalWrapper';
import { useDriverCapabilities } from '../../hooks/useDriverCapabilities';
import { supportsSchemaLessTables } from '../../LogicalModels/LogicalModelWidget/utils';

interface TableRowProps {
  dataSourceName: string;
  table: TrackableTable;
  checked: boolean;
  reset: () => void;
  onChange: () => void;
  onTableNameClick?: () => void;
  onTableTrack?: (table: TrackableTable) => void;
  isRowSelectionEnabled: boolean;
}

export const TableRow = React.memo(
  ({
    checked,
    dataSourceName,
    table,
    reset,
    onChange,
    onTableNameClick,
    onTableTrack,
    isRowSelectionEnabled,
  }: TableRowProps) => {
    const [showCustomModal, setShowCustomModal] = React.useState(false);
    const [isMongoTrackingModalVisible, setShowMongoTrackingModalVisible] =
      React.useState(false);
    const { trackTables, untrackTables, isLoading } = useTrackTables({
      dataSourceName,
    });

    const { data: capabilities } = useDriverCapabilities({
      dataSourceName: dataSourceName,
    });

    const areSchemaLessTablesSupported = supportsSchemaLessTables(capabilities);

    const track = (customConfiguration?: MetadataTable['configuration']) => {
      const t = { ...table };
      if (customConfiguration) {
        t.configuration = customConfiguration;
      }

      trackTables({
        tables: [t],
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Object tracked successfully.',
          });
          reset();
          setShowCustomModal(false);
          onTableTrack?.(table);
        },
        onError: err => {
          hasuraToast({
            type: 'error',
            title: 'Unable to perform operation',
            message: (err as APIError).message,
          });
        },
      });
    };

    const untrack = () => {
      untrackTables({
        tables: [table],
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Object untracked successfully.',
          });
          reset();
          onTableTrack?.(table);
        },
        onError: err => {
          hasuraToast({
            type: 'error',
            title: 'Unable to perform operation',
            message: (err as APIError).message,
          });
        },
      });
    };

    return (
      <CardedTable.TableBodyRow
        className={checked ? 'bg-blue-50' : 'bg-transparent'}
      >
        {isRowSelectionEnabled && (
          <td className="w-0 px-sm text-sm font-semibold text-muted uppercase tracking-wider">
            <input
              type="checkbox"
              className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
              value={table.id}
              checked={checked}
              onChange={onChange}
            />
          </td>
        )}
        <CardedTable.TableBodyCell>
          <TableDisplayName onClick={onTableNameClick} table={table.table} />
        </CardedTable.TableBodyCell>
        <CardedTable.TableBodyCell>{table.type}</CardedTable.TableBodyCell>
        <CardedTable.TableBodyCell>
          {table.is_tracked ? (
            <Button
              data-testid={`untrack-${table.name}`}
              size="sm"
              onClick={() => untrack()}
              isLoading={isLoading}
              loadingText="Please wait"
            >
              Untrack
            </Button>
          ) : (
            <div className="flex flex-row">
              <Button
                data-testid={`track-${table.name}`}
                size="sm"
                onClick={() => {
                  if (areSchemaLessTablesSupported) {
                    setShowMongoTrackingModalVisible(true);
                    return;
                  }
                  track();
                }}
                isLoading={isLoading}
                loadingText="Please wait"
              >
                Track
              </Button>

              {/* Hiding this customize button while loading as it looks odd to have two buttons in "loading mode" */}
              {!areSchemaLessTablesSupported && !isLoading && (
                <Button
                  size="sm"
                  className="ml-2"
                  onClick={() => {
                    if (areSchemaLessTablesSupported) {
                      setShowMongoTrackingModalVisible(true);
                      return;
                    }
                    setShowCustomModal(true);
                  }}
                  icon={<FiSettings />}
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
                isLoading={isLoading}
                show={showCustomModal}
                source={dataSourceName}
              />

              {isMongoTrackingModalVisible && (
                <MongoTrackCollectionModalWrapper
                  dataSourceName={dataSourceName}
                  collectionName={table.name}
                  isVisible={isMongoTrackingModalVisible}
                  onClose={() => setShowMongoTrackingModalVisible(false)}
                />
              )}
            </div>
          )}
        </CardedTable.TableBodyCell>
      </CardedTable.TableBodyRow>
    );
  }
);
