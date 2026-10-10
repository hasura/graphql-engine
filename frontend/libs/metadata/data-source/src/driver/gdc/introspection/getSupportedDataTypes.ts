import { GetSupportedDataTypesProps, TableColumnTypeMap } from '../../types';
import { getDriverCapabilities } from './getDriverCapabilities';

const getDefaultDataTypes = (): TableColumnTypeMap => {
  return {
    array: [],
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
    switch (cap.graphql_type) {
      case 'Boolean':
        result.boolean.push(key);
        break;
      case 'Float':
        result.float.push(key);
        break;
      case 'ID':
      case 'String':
        result.string.push(key);
        break;
      case 'Int':
        result.integer.push(key);
        break;
    }
  });

  return result;
}
