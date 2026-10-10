import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { jsonToString, stringToJson } from './utils/jsonString';
import { Input } from '@hasura/shared/ui';

export function ObjectValueInput({
  value,
  componentLevelId,
  path,
}: {
  value: any;
  componentLevelId: string;
  path: string[];
}) {
  const { setValue, isLoading } = useContext(rowPermissionsContext);
  return (
    <Input
      disabled={isLoading}
      data-testid={componentLevelId}
      className="rounded-md p-2 mr-4!"
      type="text"
      value={jsonToString(value)}
      onChange={(e) => {
        setValue(path, stringToJson(e.target.value));
      }}
    />
  );
}
