import { isEmpty } from 'lodash';
import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import { PermissionType } from './types';

export const Operator = ({
  operator,
  path,
  noValue,
}: {
  operator: string;
  path: string[];
  noValue?: boolean;
}) => {
  const { operators, setKey } = useContext(rowPermissionsContext);
  const { columns, table, relationships } = useContext(tableContext);
  const parent = path[path.length - 1];
  const operatorLevelId = `${path?.join('.')}-select${noValue ? '-empty' : ''}`;
  return (
    <select
      data-testid={operatorLevelId || 'root-operator-picker'}
      className="border border-gray-200 rounded-md p-2"
      value={operator}
      disabled={parent === '_where' && isEmpty(table)}
      onChange={e => {
        const type = e.target.selectedOptions[0].dataset.type as PermissionType;
        setKey({ path, key: e.target.value, type });
      }}
    >
      PermissionType
      <option value="">-</option>
      {operators.boolean?.items.length ? (
        <optgroup label="Bool operators">
          {operators.boolean.items.map((item, index) => (
            <option
              data-type="boolean"
              key={'boolean' + index}
              value={item.value}
            >
              {item.name}
            </option>
          ))}
        </optgroup>
      ) : null}
      {columns.length ? (
        <optgroup label="Columns">
          {columns.map((column, index) => (
            <option
              data-type="column"
              key={'column' + index}
              value={column.name}
            >
              {column.name}
            </option>
          ))}
        </optgroup>
      ) : null}
      {operators.exist?.items.length ? (
        <optgroup label="Exist operators">
          {operators.exist.items.map((item, index) => (
            <option data-type="exist" key={'exist' + index} value={item.value}>
              {item.name}
            </option>
          ))}
        </optgroup>
      ) : null}
      {relationships?.length ? (
        <optgroup label="Relationships">
          {relationships.map((item, index) => (
            <option
              data-type="relationship"
              key={'relationship' + index}
              value={item.name}
            >
              {item.name}
            </option>
          ))}
        </optgroup>
      ) : null}
    </select>
  );
};
