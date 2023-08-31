import { useContext } from 'react';
import { Button } from '../../../../../../../new-components/Button';
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
        <div className="p-2 ml-6">
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
          >
            Add input
          </Button>
        </div>
      </div>
    </ConditionalTableProvider>
  );
}
