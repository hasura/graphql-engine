import React from 'react';
import { FaTable } from 'react-icons/fa';

import { CardedTable } from '@/new-components/CardedTable';
import { Button } from '@/new-components/Button';
import { useTrackTable } from '../hooks/useTrackTable';
import { useUntrackTable } from '../hooks/useUntrackTable';

import { TrackableTable } from '../types';

interface RenderTableRowProps {
  dataSourceName: string;
  table: TrackableTable;
  checked: boolean;
  reset: () => void;
  onChange: () => void;
}

export const RenderTableRow = React.memo(
  ({
    checked,
    dataSourceName,
    table,
    reset,
    onChange,
  }: RenderTableRowProps) => {
    const { trackTable } = useTrackTable(dataSourceName);
    const { untrackTable } = useUntrackTable(dataSourceName);

    const track = () => {
      trackTable(table);
      reset();
    };

    const untrack = () => {
      untrackTable(table);
      reset();
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
            <Button size="sm" onClick={untrack}>
              Untrack
            </Button>
          ) : (
            <Button size="sm" onClick={track}>
              Track
            </Button>
          )}
        </CardedTable.TableBodyCell>
      </CardedTable.TableBodyRow>
    );
  }
);
