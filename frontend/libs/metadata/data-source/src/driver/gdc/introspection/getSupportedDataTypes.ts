import { GetSupportedDataTypesProps, TableColumnTypeMap } from '../../types';
import { getDriverCapabilities } from './getDriverCapabilities';
import { getScalarConsoleDataType } from './utils';

const getDefaultDataTypes = (): TableColumnTypeMap => {
  return {
    array: [],
    binary: [],
    boolean: [],
    date: [],
    float: [],
    geography: [],
    integer: [],
    json: [],
    number: [],
    string: [],
    text: [],
    time: [],
    timestamp: [],
    uuid: [],
  };
};

export async function getSupportedDataTypes(
  props: GetSupportedDataTypesProps,
): Promise<TableColumnTypeMap> {
  const capabilities = await getDriverCapabilities(props);
  const result = getDefaultDataTypes();
  if (!capabilities.scalar_types) {
    return result;
  }

  Object.entries(capabilities.scalar_types).forEach(([key, cap]) => {
    result[getScalarConsoleDataType(key, cap)].push(key);
  });

  return result;
}
