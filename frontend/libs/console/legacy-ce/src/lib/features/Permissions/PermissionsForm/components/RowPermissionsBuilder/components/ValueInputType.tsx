import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import isEmpty from 'lodash/isEmpty';
import { useOperators } from './utils/comparatorsFromSchema';
import { ObjectValueInput } from './ObjectValueInput';
import { BooleanValueInput } from './BooleanValueInput';

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
  if (operator?.type === 'jsonb' && operator?.inputStructure === 'object') {
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
        setValue(
          path,
          operator?.inputType === 'boolean'
            ? Boolean(e.target.value)
            : e.target.value
        );
      }}
    />
  );
};
