import isEmpty from 'lodash/isEmpty';
import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import { PermissionType } from './types';
import { logicalModelContext } from './RootLogicalModelProvider';
import { useForbiddenFeatures } from './ForbiddenFeaturesProvider';
import { rootTableContext } from './RootTableProvider';
import { areTablesEqual } from '../../../../../hasura-metadata-api';

export const Operator = ({
  operator,
  path,
  v,
}: {
  operator: string;
  path: string[];
  v: any;
}) => {
  const { operators, setKey, loadRelationships, isLoading } = useContext(
    rowPermissionsContext
  );
  const { tables } = useContext(rootTableContext);
  const { columns, table, relationships } = useContext(tableContext);
  const { rootLogicalModel } = useContext(logicalModelContext);
  const parent = path[path.length - 1];
  const operatorLevelId =
    path.length === 0
      ? 'root-operator'
      : `${path?.join('.')}-operator${operator ? `-root` : ''}`;
  const { hasFeature } = useForbiddenFeatures();
  return (
    <select
      data-testid={operatorLevelId}
      className="border border-gray-200 rounded-md p-2 pr-4"
      value={operator}
      disabled={isLoading || (parent === '_where' && isEmpty(table))}
      onChange={e => {
        const type = e.target.selectedOptions[0].dataset.type as PermissionType;
        if (type === 'relationship') {
          const foundTable = tables.find(t => areTablesEqual(t.table, table));
          if (foundTable) {
            loadRelationships?.(foundTable.relationships);
          }
        }
        setKey({ path, key: e.target.value, type });
      }}
    >
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
              data-type={column.dataType === 'object' ? 'object' : 'column'}
              key={'column' + index}
              value={column.name}
            >
              {column.name}
            </option>
          ))}
        </optgroup>
      ) : null}
      {rootLogicalModel?.fields.length ? (
        <optgroup label="Columns">
          {rootLogicalModel?.fields.map((field, index) => (
            <option
              data-type="column"
              key={'column' + index}
              value={field.name}
            >
              {field.name}
            </option>
          ))}
        </optgroup>
      ) : null}
      {hasFeature('exists') && operators.exist?.items.length ? (
        <optgroup label="Exist operators">
          {operators.exist.items.map((item, index) => (
            <option data-type="exist" key={'exist' + index} value={item.value}>
              {item.name}
            </option>
          ))}
        </optgroup>
      ) : null}
      {relationships.length ? (
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
