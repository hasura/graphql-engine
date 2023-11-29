import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import isEmpty from 'lodash/isEmpty';
import { useOperators } from './utils/comparatorsFromSchema';
import { ObjectValueInput } from './ObjectValueInput';
import { BooleanValueInput } from './BooleanValueInput';
import { Operator } from './types';
import { rootTableContext } from './RootTableProvider';
import { areTablesEqual } from '../../../../../hasura-metadata-api';

export const checkUseObjectInput = (
  comparatorName: string,
  operator: Operator | undefined
) => {
  if (operator?.type === 'json' || operator?.type === 'jsonb') return true;
  if (
    comparatorName === '_st_d_within' ||
    comparatorName === '_st_within' ||
    comparatorName === '_st_3d_d_within' ||
    comparatorName === '_st_contains' ||
    comparatorName === '_st_crosses' ||
    comparatorName === '_st_intersects' ||
    comparatorName === '_st_touches' ||
    comparatorName === '_st_overlaps' ||
    comparatorName === '_st_crosses'
  )
    return true;

  return false;
};

export const ValueInputType = ({
  componentLevelId,
  path,
  comparatorName,
  value,
}: {
  componentLevelId: string;
  path: string[];
  comparatorName: string;
  value: any;
}) => {
  const { setValue, isLoading } = useContext(rowPermissionsContext);
  const { tables } = useContext(rootTableContext);
  const { table } = useContext(tableContext);
  const operators = useOperators({ path });
  const operator = operators.find(o => o.name === comparatorName);

  if (operator?.inputType === 'boolean') {
    return (
      <BooleanValueInput
        componentLevelId={componentLevelId}
        path={path}
        value={value}
      />
    );
  }

  if (checkUseObjectInput(comparatorName, operator)) {
    return (
      <ObjectValueInput
        componentLevelId={componentLevelId}
        path={path}
        value={value}
      />
    );
  }

  return (
    <input
      data-testid={componentLevelId}
      disabled={isLoading || (comparatorName === '_where' && isEmpty(table))}
      className={`border border-gray-200 rounded-md p-2 !mr-4 ${
        isLoading ? 'bg-gray-100' : ''
      }`}
      type="text"
      value={value}
      onChange={e => {
        let value = e.target.value as any;
        const foundTable = tables.find(t => areTablesEqual(t.table, table));
        const column = foundTable?.columns.find(
          c => c.name === path[path.length - 2]
        );
        if (!isNaN(value) && value !== '') {
          try {
            if (column?.graphQLProperties?.scalarType === 'Int') {
              value = parseInt(value);
            } else if (column?.graphQLProperties?.scalarType === 'Float') {
              value = parseFloat(value);
            }
          } catch (e) {
            console.error(e);
            // If there is an error it means it's a string
            // This can happen, users can set values like X-Hasura-User-Id
            // We catch so we use the value as is
          }
        }
        setValue(
          path,
          operator?.inputType === 'boolean' ? Boolean(e.target.value) : value
        );
      }}
    />
  );
};
