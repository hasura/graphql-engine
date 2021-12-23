import { getDataTypeInfo } from '../Common/utils';

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
  /*
   * alterTypeOptions can also only contain only three elements
   */
  let allInfo = [...currentInfo];
  let allOptionsMap = {
    ...currentMap,
  };

  if (alterTypeOptions.length > 3) {
    const {
      typInfo: validOptions,
      typValueMap: validOptionsMap,
    } = getDataTypeInfo(alterTypeOptions.slice(3, 6), colName, 0);

    allInfo = allInfo.concat(validOptions);
    // const allInfo = [...currentInfo, ...validOptions];
    allOptionsMap = {
      ...validOptionsMap,
      ...currentMap,
    };
  }

  return {
    alterOptions: allInfo,
    alterOptionsValueMap: allOptionsMap,
  };
};

export const convertToArrayOptions = options => {
  return options.map(opt => ({
    value: opt.value + '[]',
    label: opt.label + '[]',
  }));
};

export const sanitiseRootFields = rootFields => {
  const santisedRootFields = {};
  Object.keys(rootFields).forEach(rootFieldType => {
    let rootField = rootFields[rootFieldType];
    if (rootField !== null) {
      rootField = rootField.trim() || null;
    }
    santisedRootFields[rootFieldType] = rootField;
  });
  return santisedRootFields;
};

export const sanitiseColumnNames = columnNames => {
  const sanitised = {};
  Object.keys(columnNames).forEach(c => {
    const trimmedCustomName = columnNames[c] ? columnNames[c].trim() : null;
    if (trimmedCustomName) {
      sanitised[c] = trimmedCustomName;
    }
  });
  return sanitised;
};

export { convertArrayToJson, getValidAlterOptions };
