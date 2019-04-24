import { getDataTypeInfo } from '../Add/utils';

const convertArrayToJson = (arr, keyIndex = 0) => {
  const converted = {};
  arr.forEach(a => {
    converted[a[keyIndex]] = a;
  });
  return converted;
};

const getValidAlterOptions = (alterTypeOptions, colName) => {
  const { typInfo: currentInfo, typValueMap: currentMap } = getDataTypeInfo(
    alterTypeOptions.slice(0, 3),
    colName,
    0
  );

  const {
    typInfo: validOptions,
    typValueMap: validOptionsMap,
  } = getDataTypeInfo(alterTypeOptions.slice(3, 6), colName, 0);

  const allInfo = [...currentInfo, ...validOptions];
  const allOptionsMap = {
    ...validOptionsMap,
    ...currentMap,
  };
  return {
    alterOptions: allInfo,
    alterOptionsValueMap: allOptionsMap,
  };
};

export { convertArrayToJson, getValidAlterOptions };
