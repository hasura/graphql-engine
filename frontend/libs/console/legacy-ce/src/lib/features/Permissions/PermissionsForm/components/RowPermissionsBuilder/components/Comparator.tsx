import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { useOperators } from './utils/comparatorsFromSchema';

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
    <select
      data-testid={comparatorLevelId}
      className="border border-gray-200 rounded-md p-2"
      value={comparator}
      onChange={e => {
        setKey({ path, key: e.target.value, type: 'comparator' });
      }}
    >
      <option value="">-</option>
      {operators.map((o, index) => (
        <option key={index} value={o.name}>
          {o.name}
        </option>
      ))}
    </select>
  );
};
