import { useContext, useState } from 'react';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import isEmpty from 'lodash/isEmpty';
import { useOperators } from './utils/comparatorsFromSchema';
import { ObjectValueInput } from './ObjectValueInput';
import { BooleanValueInput } from './BooleanValueInput';
import { Operator } from './types';
import { rootTableContext } from './RootTableProvider';
import { findTable } from './utils/tables';
import { Input } from '@hasura/shared/ui';

export const checkUseObjectInput = (
  comparatorName: string,
  operator: Operator | undefined,
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
  const { table, source } = useContext(tableContext);
  const [draft, setDraft] = useState<string | null>(null);
  const operators = useOperators({ path });
  const operator = operators.find((o) => o.name === comparatorName);

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

  const scalarType = findTable(tables, table, source)?.columns.find(
    (c) => c.name === path[path.length - 2],
  )?.graphQLProperties?.scalarType;

  // a partly typed number (e.g. `1.`) is shown as typed, as long as it
  // still stands for the value
  const displayedValue =
    draft !== null && toColumnValue(draft, scalarType) === value
      ? draft
      : value;

  return (
    <Input
      data-testid={componentLevelId}
      disabled={isLoading || (comparatorName === '_where' && isEmpty(table))}
      className={`p-2 mr-4!`}
      type="text"
      value={displayedValue}
      onChange={(e) => {
        setDraft(e.target.value);
        setValue(path, toColumnValue(e.target.value, scalarType));
      }}
    />
  );
};

const INT_PATTERN = /^-?\d+$/;
const FLOAT_PATTERN = /^-?\d+(\.\d+)?([eE][-+]?\d+)?$/;

/**
 * Numbers are stored as numbers once they're complete, anything else (e.g.
 * `X-Hasura-User-Id`, or `1.` while typing `1.5`) as text.
 */
export const toColumnValue = (text: string, scalarType: string | undefined) => {
  if (scalarType === 'Int' && INT_PATTERN.test(text)) return parseInt(text, 10);
  if (scalarType === 'Float' && FLOAT_PATTERN.test(text))
    return parseFloat(text);
  return text;
};
