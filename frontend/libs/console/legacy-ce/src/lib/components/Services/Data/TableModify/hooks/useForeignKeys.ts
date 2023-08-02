import { dataSource, Table } from '../../../../../dataSources';
import { useAppDispatch } from '../../../../../storeHooks';
import { useEffect } from 'react';
import { getExistingFKConstraints } from '../../Common/Components/utils';
import { updateSchemaInfo } from '../../DataActions';
import { ordinalColSort } from '../../utils';
import { setForeignKeys } from '../ModifyActions';

type UseForeignKeysProps = {
  table: Table | undefined;
};

export const useForeignKeys = ({ table }: UseForeignKeysProps) => {
  const dispatch = useAppDispatch();
  const columns = table?.columns?.sort(ordinalColSort) || [];

  const orderedColumns = columns.map((c, i) => ({
    name: c.column_name,
    index: i,
  }));

  const existingForeignKeys = getExistingFKConstraints(table, orderedColumns);
  const schemasToBeFetched: Record<string, boolean> = {};
  existingForeignKeys.forEach((efk: { refSchemaName: string }) => {
    schemasToBeFetched[efk.refSchemaName] = true;
  });

  existingForeignKeys.push({
    onUpdate: dataSource?.violationActions?.[0],
    onDelete: dataSource?.violationActions?.[0],
    colMappings: [{ column: '', refColumn: '' }],
  });

  useEffect(() => {
    dispatch(setForeignKeys(existingForeignKeys));
    dispatch(updateSchemaInfo({ schemas: Object.keys(schemasToBeFetched) }));
  }, []);

  return {
    orderedColumns,
    existingForeignKeys,
  };
};
