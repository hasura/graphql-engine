import { useContext } from 'react';
import { allOperators } from '@/components/Common/FilterQuery/utils';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';

const defaultOperators = allOperators.map(o => ({
  name: o.name,
  operator: o.alias,
}));

export const Comparator = ({
  comparator,
  path,
  noValue,
}: {
  comparator: string;
  path: string[];
  noValue?: boolean;
}) => {
  const { setKey, comparators } = useContext(rowPermissionsContext);
  const comparatorLevelId = `${path?.join('.')}-select${
    noValue ? '-is-empty' : ''
  }`;
  const { columns } = useContext(tableContext);
  const columnName = path[path.length - 2];
  const column = columns.find(c => c.name === columnName);
  const operators =
    column?.type && comparators[column.type]?.operators
      ? comparators[column.type].operators
      : defaultOperators;

  return (
    <select
      data-testid={comparatorLevelId}
      className="border border-gray-200 rounded-md p-2"
      value={comparator}
      onChange={e => {
        setKey({ path, key: e.target.value, type: 'comparator' });
      }}
    >
      {operators.map((o, index) => (
        <option key={index} value={o.operator}>
          {o.name}
        </option>
      ))}
    </select>
  );
};
