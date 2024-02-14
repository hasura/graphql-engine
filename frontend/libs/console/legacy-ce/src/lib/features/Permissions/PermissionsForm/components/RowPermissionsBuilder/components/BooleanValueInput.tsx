import { useContext } from 'react';
import { Table } from '../../../../../hasura-metadata-types';
import { rowPermissionsContext } from './RowPermissionsProvider';

export function BooleanValueInput({
  path,
  value,
  componentLevelId,
}: {
  value: any;
  componentLevelId: string;
  path: string[];
}) {
  const { setValue, isLoading } = useContext(rowPermissionsContext);
  return (
    <div className="flex">
      <select
        disabled={isLoading}
        data-testid={componentLevelId}
        className="border border-gray-200 rounded-md"
        value={JSON.stringify(value)}
        defaultValue={JSON.parse(value) ?? false}
        onChange={e => {
          setValue(path, JSON.parse(e.target.value) as Table);
        }}
      >
        <option key="false" value="false">
          False
        </option>
        <option key="true" value="true">
          True
        </option>
      </select>
    </div>
  );
}
