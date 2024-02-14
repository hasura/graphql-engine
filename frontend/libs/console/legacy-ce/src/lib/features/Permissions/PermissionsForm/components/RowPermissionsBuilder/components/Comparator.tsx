import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { useOperators } from './utils/comparatorsFromSchema';
import Select, { components } from 'react-select';
import { FiChevronDown } from 'react-icons/fi';
import clsx from 'clsx';

export const Comparator = ({
  comparator,
  v,
  path,
}: {
  comparator: string;
  v: any;
  path: string[];
}) => {
  const { setKey, isLoading } = useContext(rowPermissionsContext);
  const comparatorLevelId = `${path?.join('.')}-comparator`;
  const operators = useOperators({ path });

  return (
    <Select
      isDisabled={isLoading}
      inputId={`${comparatorLevelId}-select-value`}
      isSearchable
      aria-label={comparatorLevelId}
      components={{
        DropdownIndicator: props => {
          const { className } = props;
          return (
            <components.DropdownIndicator
              {...props}
              className={clsx(className, '!text-gray-500 hover:!text-gray-500')}
            >
              <FiChevronDown className="w-5 h-5" />
            </components.DropdownIndicator>
          );
        },
        IndicatorSeparator: () => null,
      }}
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
