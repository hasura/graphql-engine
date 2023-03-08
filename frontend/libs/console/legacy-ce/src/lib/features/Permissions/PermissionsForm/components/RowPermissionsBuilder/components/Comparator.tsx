import { useContext } from 'react';
import { allOperators } from '../../../../../../components/Services/Data/TablePermissions/PermissionBuilder/utils';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';

const defaultOperators = allOperators.map(operator => ({
  name: operator,
  operator,
}));

export const Comparator = ({
  comparator,
  v,
  path,
}: {
  comparator: string;
  v: any;
  path: string[];
}) => {
  const { setKey, comparators } = useContext(rowPermissionsContext);
  const comparatorLevelId = `${path?.join('.')}-comparator`;
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
          {o.operator}
        </option>
      ))}
    </select>
  );
};
