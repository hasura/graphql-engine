import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { Flex } from '@radix-ui/themes';
import { Select } from '@hasura/shared/ui';

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
    <Flex>
      <Select
        disabled={isLoading}
        data-testid={componentLevelId}
        value={JSON.stringify(value)}
        onChange={(value) => {
          setValue(path, JSON.stringify(value));
        }}
        options={['false', 'true'].map((v) => ({
          value: v,
          label: v,
        }))}
      />
    </Flex>
  );
}
