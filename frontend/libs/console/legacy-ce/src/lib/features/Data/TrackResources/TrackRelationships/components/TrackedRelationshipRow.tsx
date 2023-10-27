import { ChangeEvent } from 'react';
import { FaColumns, FaDatabase, FaTable } from 'react-icons/fa';
import { capitaliseFirstLetter } from '../../../../../components/Common/ConfigureTransformation/utils';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { getTableDisplayName } from '../../../../DatabaseRelationships';
import { RelationshipIcon } from '../../../../DatabaseRelationships/components/RelationshipIcon';
import { TrackedSuggestedRelationship } from '../types';

export type TrackedRelationshipRowProps = {
  dataSourceName: string;
  isChecked: boolean;
  isLoading: boolean;
  onToggle: (e: ChangeEvent<HTMLInputElement>) => void;
  onUntrack: () => void;
  onRename: () => void;
  relationship: TrackedSuggestedRelationship;
};

export const TrackedRelationshipRow: React.VFC<TrackedRelationshipRowProps> = ({
  dataSourceName,
  isChecked,
  isLoading,
  onUntrack,
  onToggle,
  onRename,
  relationship,
}) => {
  return (
    <CardedTable.TableBodyRow
      className={isChecked ? 'bg-blue-50' : 'bg-transparent'}
    >
      <td className="w-0 px-sm text-sm font-semibold text-muted uppercase tracking-wider">
        <input
          type="checkbox"
          className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
          value={relationship.name}
          checked={isChecked}
          onChange={onToggle}
        />
      </td>
      <CardedTable.TableBodyCell>{relationship.name}</CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        <div className="flex items-center gap-2">
          <FaDatabase /> <span>{dataSourceName}</span>
        </div>
      </CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        {capitaliseFirstLetter(relationship.type)}
      </CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        <DisplaySuggestedRelationship relationship={relationship} />
      </CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        <div className="flex flex-row">
          <Button size="sm" onClick={onUntrack} isLoading={isLoading}>
            Untrack
          </Button>
          <Button
            size="sm"
            className="ml-1"
            onClick={() => onRename()}
            isLoading={isLoading}
          >
            Rename
          </Button>
        </div>
      </CardedTable.TableBodyCell>
    </CardedTable.TableBodyRow>
  );
};

export const DisplaySuggestedRelationship = ({
  relationship,
}: {
  relationship: TrackedSuggestedRelationship;
}) => (
  <div className="flex flex-row items-center text-sm text-muted gap-2">
    <FaTable />
    <span>{getTableDisplayName(relationship.fromTable)}</span>
    /
    <FaColumns />
    <span>{Object.keys(relationship.columnMapping ?? {}).join(' ')}</span>
    <RelationshipIcon
      type={relationship.type === 'array' ? 'one-to-many' : 'one-to-one'}
    />
    <FaTable />
    <span>{getTableDisplayName(relationship.toTable)}</span>
    /
    <FaColumns />
    {Object.values(relationship.columnMapping ?? {}).join(' ')}
  </div>
);
