import { useContext } from 'react';
import { Flex } from '@radix-ui/themes';
import { Button } from '@hasura/shared/ui';
import { isComparator } from '../utils/helpers';
import { ValueInput } from '../ValueInput';
import { rowPermissionsContext } from '../RowPermissionsProvider';
import { ConditionalTableProvider } from './ConditionalTableProvider';

export function ArrayEntry({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) {
  const { setValue } = useContext(rowPermissionsContext);

  const array = Array.isArray(v) ? v : [];
  return (
    <ConditionalTableProvider path={path}>
      <div
        className={
          !isComparator(k) ? `border-dashed border-l border-gray-200` : ''
        }
      >
        <Flex align="center" className="p-2 ml-6">
          {array.map((entry, i) => {
            return (
              <ValueInput
                key={String(i)}
                value={entry}
                path={[...path, String(i)]}
              />
            );
          })}

          <Button
            onClick={() => setValue([...path, String(array.length)], '')}
            mode="default"
            size="sm"
          >
            Add input
          </Button>
        </Flex>
      </div>
    </ConditionalTableProvider>
  );
}
