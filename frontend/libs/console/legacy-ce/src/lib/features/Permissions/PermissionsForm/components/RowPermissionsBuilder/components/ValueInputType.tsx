import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import isEmpty from 'lodash/isEmpty';
import { useOperators } from './utils/comparatorsFromSchema';
import { ObjectValueInput } from './ObjectValueInput';
import { BooleanValueInput } from './BooleanValueInput';
import { Operator } from './types';

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
  const { setValue } = useContext(rowPermissionsContext);
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
      disabled={comparatorName === '_where' && isEmpty(table)}
      className="border border-gray-200 rounded-md p-2 !mr-4"
      type="text"
      value={value}
      onChange={e => {
        let value = e.target.value as any;
        if (!isNaN(value) && value !== '') {
          value = parseInt(value);
        }
        setValue(
          path,
          operator?.inputType === 'boolean' ? Boolean(e.target.value) : value
        );
      }}
    />
  );
};
