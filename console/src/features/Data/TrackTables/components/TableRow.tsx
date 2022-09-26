import { TableTrackingCustomizationModal } from '@/components/Services/Data/Schema/tableTrackCustomization/TableTrackingCustomizationModal';
import { MetadataTableConfig } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { CardedTable } from '@/new-components/CardedTable';
import React from 'react';
import { FaTable } from 'react-icons/fa';
import { FiSettings } from 'react-icons/fi';
import { useTrackTable } from '../hooks/useTrackTable';
import { TrackableTable } from '../types';

interface TableRowProps {
  dataSourceName: string;
  table: TrackableTable;
  checked: boolean;
  reset: () => void;
  onChange: () => void;
}

export const TableRow = React.memo(
  ({ checked, dataSourceName, table, reset, onChange }: TableRowProps) => {
    const [showCustomModal, setShowCustomModal] = React.useState(false);
    const { trackTable, untrackTable, loading } = useTrackTable(dataSourceName);

    const track = (customConfiguration?: MetadataTableConfig) => {
      const t = { ...table };
      if (customConfiguration) {
        t.configuration = customConfiguration;
      }
      trackTable(t).then(() => {
        reset();
        setShowCustomModal(false);
      });
    };

    const untrack = () => {
      untrackTable(table).then(() => {
        reset();
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
          <FaTable className="text-sm text-muted mr-xs" />
          {table.name}
        </CardedTable.TableBodyCell>
        <CardedTable.TableBodyCell>{table.type}</CardedTable.TableBodyCell>
        <CardedTable.TableBodyCell>
          {table.is_tracked ? (
            <Button size="sm" onClick={untrack} isLoading={loading}>
              Untrack
            </Button>
          ) : (
            <div className="flex flex-row">
              <Button size="sm" onClick={() => track()} isLoading={loading}>
                Track
              </Button>
              {/* Hiding this customize button while loading as it looks odd to have two buttons in "loading mode" */}
              {!loading && (
                <Button
                  size="sm"
                  className="ml-2"
                  onClick={() => setShowCustomModal(true)}
                  icon={<FiSettings />}
                >
                  Customize &amp; Track
                </Button>
              )}

              <TableTrackingCustomizationModal
                tableName={table.name}
                onSubmit={(formValues, config) => {
                  track(config);
                }}
                onClose={() => {
                  setShowCustomModal(false);
                }}
                isLoading={loading}
                show={showCustomModal}
              />
            </div>
          )}
        </CardedTable.TableBodyCell>
      </CardedTable.TableBodyRow>
    );
  }
);
