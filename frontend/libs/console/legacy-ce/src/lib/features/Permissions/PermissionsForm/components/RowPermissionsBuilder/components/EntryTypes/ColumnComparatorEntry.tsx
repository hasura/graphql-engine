import { useContext, useEffect } from 'react';
import { isComparator } from '../utils/helpers';
import { tableContext } from '../TableProvider';
import { rowPermissionsContext } from '../RowPermissionsProvider';
import { ConditionalTableProvider } from './ConditionalTableProvider';
import { rootTableContext } from '../RootTableProvider';

export function ColumnComparatorEntry({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) {
  const { setValue } = useContext(rowPermissionsContext);
  const showRootColumns = v.includes('$');
  const stringifiedValue = JSON.stringify(v);
  useEffect(() => {
    if (!Array.isArray(v)) {
      setValue(path, []);
    }
  }, [stringifiedValue, setValue]);
  return (
    <ConditionalTableProvider path={path}>
      <div
        className={
          !isComparator(k) ? `border-dashed border-l border-gray-200` : ''
        }
      >
        <div className="ml-6">
          <div className="p-2 space-y-2">
            <ColumnsSelect v={v} k={k} path={path} />
            {showRootColumns ? (
              <RootColumnsSelect v={v} k={k} path={path} />
            ) : null}
          </div>
        </div>
      </div>
    </ConditionalTableProvider>
  );
}

function ColumnsSelect({ k, v, path }: { k: string; v: any; path: string[] }) {
  const { setValue } = useContext(rowPermissionsContext);
  const { columns } = useContext(tableContext);
  const value =
    v.length === 1
      ? v[0]
      : v.length === 0
      ? ''
      : // Could be $ or undefined
        v.find((v: any) => v === '$');
  const testId = `${path.join('.')}-column-comparator-entry`;

  return (
    <select
      data-testid={testId}
      className="border border-gray-200 rounded-md block"
      value={value}
      onChange={e => {
        if (e.target.value === '$') {
          setValue(path, [e.target.value, '']);
        } else {
          setValue(path, [e.target.value]);
        }
      }}
    >
      <option value="">-</option>
      <optgroup label="columns">
        {columns.map(c => {
          return (
            <option
              data-testid={`${testId}-${c.name}`}
              key={c.name}
              value={c.name}
            >
              {c.name}
            </option>
          );
        })}
      </optgroup>
      <optgroup label="root">
        <option data-testid={`${testId}-$`} value="$">
          $
        </option>
      </optgroup>
    </select>
  );
}

function RootColumnsSelect({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) {
  const { setValue } = useContext(rowPermissionsContext);
  const value = v.find((v: any) => v !== '$');
  const { rootTable } = useContext(rootTableContext);
  const testId = `${path.join('.')}-root-column-comparator-entry`;
  return (
    <select
      className="border border-gray-200 rounded-md block"
      data-testid={testId}
      value={value}
      onChange={e => {
        setValue(path, ['$', e.target.value]);
      }}
    >
      <option value="">-</option>
      {rootTable?.columns.map(c => {
        return (
          <option data-testid={`${testId}-${c.name}`} value={c.name}>
            {c.name}
          </option>
        );
      })}
    </select>
  );
}
