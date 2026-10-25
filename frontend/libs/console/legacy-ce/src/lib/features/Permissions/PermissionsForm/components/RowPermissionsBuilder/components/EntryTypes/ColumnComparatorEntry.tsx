import { useContext, useEffect } from 'react';
import { GroupBase } from 'react-select';
import { ReactSelect, ReactSelectOptionType } from '@hasura/shared/ui';
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
  // the value is a column path; anything else (e.g. typed in the JSON editor)
  // is reset to an empty path
  const valueIsArray = Array.isArray(v);
  const columnPath: string[] = valueIsArray ? v : [];
  const showRootColumns = columnPath.includes('$');
  useEffect(() => {
    if (!valueIsArray) {
      setValue(path, []);
    }
  }, [valueIsArray, path, setValue]);
  return (
    <ConditionalTableProvider path={path}>
      <div
        className={
          !isComparator(k) ? `border-dashed border-l border-gray-200` : ''
        }
      >
        <div className="ml-6">
          <div className="p-2 space-y-2">
            <ColumnsSelect v={columnPath} k={k} path={path} />
            {showRootColumns ? (
              <RootColumnsSelect v={columnPath} k={k} path={path} />
            ) : null}
          </div>
        </div>
      </div>
    </ConditionalTableProvider>
  );
}

/**
 * A searchable select of columns. The `testId` is kept on a wrapper, as
 * react-select doesn't forward it, and each option of the open menu gets
 * `${testId}-${column}`.
 */
function ColumnSelect({
  testId,
  value,
  options,
  onChange,
}: {
  testId: string;
  value: string | undefined;
  options: GroupBase<ReactSelectOptionType<string>>[];
  onChange: (value: string) => void;
}) {
  const { isLoading } = useContext(rowPermissionsContext);
  const selectedOption =
    options
      .flatMap((group) => group.options)
      .find((option) => option.value === value) ?? null;

  return (
    <div data-testid={testId} className="w-60">
      <ReactSelect<ReactSelectOptionType<string>>
        inputId={`${testId}-select-value`}
        aria-label={testId}
        isSearchable
        isDisabled={isLoading}
        placeholder="-"
        value={selectedOption}
        options={options}
        formatOptionLabel={(option, { context }) =>
          context === 'menu' ? (
            <span data-testid={`${testId}-${option.value}`}>
              {option.label}
            </span>
          ) : (
            option.label
          )
        }
        onChange={(option) => {
          if (option) onChange(option.value);
        }}
      />
    </div>
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

  return (
    <ColumnSelect
      testId={`${path.join('.')}-column-comparator-entry`}
      value={value}
      options={[
        {
          label: 'Columns',
          options: columns.map((c) => ({ value: c.name, label: c.name })),
        },
        {
          label: 'Root table',
          options: [{ value: '$', label: '$' }],
        },
      ]}
      onChange={(column) => {
        setValue(path, column === '$' ? ['$', ''] : [column]);
      }}
    />
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

  return (
    <ColumnSelect
      testId={`${path.join('.')}-root-column-comparator-entry`}
      value={value}
      options={[
        {
          label: 'Root table columns',
          options: (rootTable?.columns ?? []).map((c) => ({
            value: c.name,
            label: c.name,
          })),
        },
      ]}
      onChange={(column) => {
        setValue(path, ['$', column]);
      }}
    />
  );
}
