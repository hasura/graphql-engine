import isEmpty from 'lodash/isEmpty';
import { Button } from '../../../../../../new-components/Button';
import { isComparator } from './utils';
import { useContext } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import { useOperators } from './utils/comparatorsFromSchema';

export function InputSuggestion({
  comparatorName,
  path,
  componentLevelId,
}: {
  comparatorName: string;
  componentLevelId: string;
  path: string[];
}) {
  const { setValue } = useContext(rowPermissionsContext);
  const { table } = useContext(tableContext);
  const operators = useOperators({ path });
  const operator = operators.find(o => o.name === comparatorName);

  if (comparatorName === '_contains' || comparatorName === '_contained_in') {
    return (
      <Button onClick={() => setValue(path, {})} mode="default">
        [JSON]
      </Button>
    );
  }
  if (operator?.inputType !== 'boolean' && isComparator(comparatorName)) {
    return (
      <Button
        disabled={comparatorName === '_where' && isEmpty(table)}
        onClick={() => setValue(path, 'X-Hasura-User-Id')}
        data-testid={`${componentLevelId}-x-hasura-user-id`}
        mode="default"
      >
        [x-hasura-user-id]
      </Button>
    );
  }
  return null;
}
