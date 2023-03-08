import { useContext } from 'react';
import { Button } from '../../../../../../../new-components/Button';
import { isComparator } from '../utils/helpers';
import { tableContext } from '../TableProvider';
import { typesContext } from '../TypesProvider';
import { ValueInput } from '../ValueInput';
import { rowPermissionsContext } from '../RowPermissionsProvider';
import { createWrapper } from './utils';

export function ArrayEntry({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) {
  const { types } = useContext(typesContext);
  const { relationships } = useContext(tableContext);
  const { setValue } = useContext(rowPermissionsContext);

  const Wrapper = createWrapper({
    types,
    path,
    relationships,
  });
  return (
    <Wrapper>
      <div
        className={
          !isComparator(k) ? `border-dashed border-l border-gray-200` : ''
        }
      >
        {Array.isArray(v) ? (
          <div className="p-2 ml-6">
            {v.map((val, i) => {
              return (
                <ValueInput
                  key={String(i)}
                  value={val}
                  path={[...path, String(i)]}
                />
              );
            })}

            <Button
              onClick={() => setValue([...path, String(v.length)], '')}
              mode="default"
            >
              Add input
            </Button>
          </div>
        ) : null}
      </div>
    </Wrapper>
  );
}
