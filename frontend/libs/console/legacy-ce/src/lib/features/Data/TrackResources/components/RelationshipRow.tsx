import { ChangeEvent, useState } from 'react';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { FaArrowRight, FaColumns, FaDatabase, FaTable } from 'react-icons/fa';
import { capitaliseFirstLetter } from '../../../../components/Common/ConfigureTransformation/utils';
import { getTableDisplayName } from '../../../DatabaseRelationships';
import { SuggestedRelationshipWithName } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';

export type RelationshipRowProps = {
  dataSourceName: string;
  isChecked: boolean;
  isLoading: boolean;
  onCustomize: () => void;
  onToggle: (e: ChangeEvent<HTMLInputElement>) => void;
  onTrack: () => Promise<void>;
  relationship: SuggestedRelationshipWithName;
};

export const RelationshipRow: React.VFC<RelationshipRowProps> = ({
  dataSourceName,
  isChecked,
  isLoading,
  onCustomize,
  onToggle,
  onTrack,
  relationship,
}) => {
  const [isLoadingState, setLoading] = useState(false);
  const onTrackHandler = () => {
    setLoading(true);
    onTrack().finally(() => setLoading(false));
  };
  return (
    <CardedTable.TableBodyRow
      className={isChecked ? 'bg-blue-50' : 'bg-transparent'}
    >
      <td className="w-0 px-sm text-sm font-semibold text-muted uppercase tracking-wider">
        <input
          type="checkbox"
          className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
          value={relationship.constraintName}
          checked={isChecked}
          onChange={onToggle}
        />
      </td>
      <CardedTable.TableBodyCell>
        {relationship.constraintName}
      </CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        <div className="flex items-center gap-2">
          <FaDatabase /> <span>{dataSourceName}</span>
        </div>
      </CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        {capitaliseFirstLetter(relationship.type)}
      </CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        <div className="flex flex-row items-center gap-2">
          <FaTable />
          <span>{getTableDisplayName(relationship.from.table)}</span>
          /
          <FaColumns />
          {relationship.from.columns.join(' ')}
          <FaArrowRight />
          <FaTable />
          <span>{getTableDisplayName(relationship.to.table)}</span>
          /
          <FaColumns />
          {relationship.to.columns.join(' ')}
        </div>
      </CardedTable.TableBodyCell>
      <CardedTable.TableBodyCell>
        <div className="flex flex-row">
          <Button
            size="sm"
            onClick={() => onTrackHandler()}
            isLoading={isLoading || isLoadingState}
          >
            Track
          </Button>
          <Button
            size="sm"
            className="ml-1"
            onClick={() => onCustomize()}
            isLoading={isLoading}
          >
            Customize
          </Button>
        </div>
      </CardedTable.TableBodyCell>
    </CardedTable.TableBodyRow>
  );
};
