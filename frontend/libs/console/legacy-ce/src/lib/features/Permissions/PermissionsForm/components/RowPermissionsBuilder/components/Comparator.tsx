import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { useOperators } from './utils/comparatorsFromSchema';
import Select from 'react-select';

export const Comparator = ({
  comparator,
  v,
  path,
}: {
  comparator: string;
  v: any;
  path: string[];
}) => {
  const { setKey } = useContext(rowPermissionsContext);
  const comparatorLevelId = `${path?.join('.')}-comparator`;
  const operators = useOperators({ path });

  return (
    <Select
      inputId={`${comparatorLevelId}-select-value`}
      isSearchable
      aria-label={comparatorLevelId}
      components={{ DropdownIndicator: null }}
      options={operators.map(o => ({
        value: o.name,
        label: o.name,
      }))}
      onChange={option => {
        const { value } = option as { value: string };
        setKey({ path, key: value, type: 'comparator' });
      }}
      defaultValue={{
        value: comparator,
        label: comparator,
      }}
      value={{
        value: comparator,
        label: comparator,
      }}
      styles={{
        control: base => ({
          ...base,
          border: 0,
          minHeight: 'auto',
        }),
      }}
      className="w-32 border border-gray-200 rounded-md"
    />
  );
};
