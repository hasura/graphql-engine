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

interface TableRowProps {
  dataSourceName: string;
  table: TrackableTable;
  checked: boolean;
  reset: () => void;
  onChange: () => void;
  onTableNameClick?: () => void;
}

export const TableRow = React.memo(
  ({
    checked,
    dataSourceName,
    table,
    reset,
    onChange,
    onTableNameClick,
  }: TableRowProps) => {
    const [showCustomModal, setShowCustomModal] = React.useState(false);
    const { trackTables, untrackTables, isLoading } = useTrackTables({
      dataSourceName,
    });

    const track = (customConfiguration?: MetadataTable['configuration']) => {
      const t = { ...table };
      if (customConfiguration) {
        t.configuration = customConfiguration;
      }

      trackTables({
        tablesToBeTracked: [t],
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Object tracked successfully.',
          });
          reset();
          setShowCustomModal(false);
        },
        onError: err => {
          console.log('!!!', err);
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
        tablesToBeUntracked: [table],
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Object untracked successfully.',
          });
          reset();
        },
        onError: err => {
          console.log('log!!');
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
        <td className="w-0 px-sm text-sm font-semibold text-muted uppercase tracking-wider">
          <input
            type="checkbox"
            className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
            value={table.id}
            checked={checked}
            onChange={onChange}
          />
        </td>
        <CardedTable.TableBodyCell>
          <TableDisplayName onClick={onTableNameClick} table={table.table} />
        </CardedTable.TableBodyCell>
        <CardedTable.TableBodyCell>{table.type}</CardedTable.TableBodyCell>
        <CardedTable.TableBodyCell>
          {table.is_tracked ? (
            <Button
              data-testid={`untrack-${table.name}`}
              size="sm"
              onClick={untrack}
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
                onClick={() => track()}
                isLoading={isLoading}
                loadingText="Please wait"
              >
                Track
              </Button>

              {/* Hiding this customize button while loading as it looks odd to have two buttons in "loading mode" */}
              {!isLoading && (
                <Button
                  size="sm"
                  className="ml-2"
                  onClick={() => setShowCustomModal(true)}
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
              />
            </div>
          )}
        </CardedTable.TableBodyCell>
      </CardedTable.TableBodyRow>
    );
  }
);
