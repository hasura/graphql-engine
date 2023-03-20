import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { jsonToString, stringToJson } from './utils/jsonString';

export function ObjectValueInput({
  value,
  componentLevelId,
  path,
}: {
  value: any;
  componentLevelId: string;
  path: string[];
}) {
  const { setValue } = useContext(rowPermissionsContext);
  return (
    <input
      data-testid={componentLevelId}
      className="border border-gray-200 rounded-md p-2 !mr-4"
      type="text"
      value={jsonToString(value)}
      onChange={e => {
        setValue(path, stringToJson(e.target.value));
      }}
    />
  );
}
