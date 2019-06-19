import { aggCategory, pgCategoryCode } from './PgInfo';

const commonlyUsedFunctions = ['now', 'gen_random_uuid', 'random'];

const getParanthesized = name => {
  return `${name}()`;
};

const splitDbRow = row => {
  /* Splits comma seperated type names
   * Splits comma seperated type user friendly type names
   * Splits comma seperated type descriptions
   * */
  return {
    typInfo: row[0].split(','),
    typDisplayName: row[1].split(','),
    typDescription: row[2].split(':'),
  };
};

/*
 * Returns a map of all data types and their source info
 * Example:
 *  For text: text text "variable-length string, no limit specified"
 * */

const getAllDataTypeMap = allDataTypes => {
  const dTIndex = {};
  allDataTypes.forEach(dataTypes => {
    const { typInfo, typDisplayName, typDescription } = splitDbRow(dataTypes);

    typInfo.forEach((currentType, typIndex) => {
      dTIndex[currentType] = [
        typInfo[typIndex],
        typDisplayName[typIndex],
        typDescription[typIndex],
      ];
    });
  });
  return dTIndex;
};

const getDataTypeInfo = (row, categoryInfo, colId, cached = {}) => {
  const columnTypeValueMap = {};

  const { typInfo, typDisplayName, typDescription } = splitDbRow(row);

  // Create option object for every valid type
  const currTypeObj = [];
  typInfo.forEach((t, i) => {
    /* Don't add types which are part of frequently used types */
    if (!(t in cached)) {
      const optObj = {
        value: t,
        label: typDisplayName[i],
        key: `${categoryInfo}_${i}`,
        colIdentifier: colId,
        description: typDescription[i],
      };
      // Memoizing option for later use
      columnTypeValueMap[t] = optObj;
      currTypeObj.push(optObj);
    }
  });
  return { typInfo: currTypeObj, typValueMap: columnTypeValueMap };
};

const getDefaultFunctionsOptions = (funcs, identifier) => {
  const defaultValues = [
    {
      title: 'All Functions',
      suggestions: [],
    },
  ];
  funcs.forEach((f, i) => {
    const funcVal = getParanthesized(f);
    const suggestionObj = {
      value: funcVal,
      label: funcVal,
      description: funcVal,
      key: i,
      colIdentifier: identifier,
      title: 'All Functions',
    };
    if (commonlyUsedFunctions.indexOf(f) !== -1) {
      if (defaultValues.length === 1) {
        defaultValues.push({
          title: 'Frequently Used Functions',
          suggestions: [],
        });
      }
      defaultValues[1].suggestions.push(suggestionObj);
    } else {
      defaultValues[0].suggestions.push(suggestionObj);
    }
  });
  /* Reversing the array just so that if frequently used types were present, they come first */
  return defaultValues.reverse();
};

/*
 * Input arguments:
 *  dataTypes -> Frequently used types
 *  , restTypes -> Information queried from database
 *  , identifier -> Identifies where this column to be tracked
 * Output:
 *  1) Type -> grouped option
 *  2) returns array of `grouped` options
 * */
const getDataOptions = (commonDataTypes, restTypes, identifier) => {
  let columnTypeValueMap = {};
  const columnDataTypes = [];
  const mainOpts = [];
  commonDataTypes.forEach((d, dKey) => {
    mainOpts.push({
      value: d.value,
      label: d.name,
      description: d.description,
      key: dKey,
      colIdentifier: identifier,
    });
    columnTypeValueMap[d.value] = mainOpts[mainOpts.length - 1];
  });
  columnDataTypes.push({
    label: 'Frequently Used Types',
    options: mainOpts,
  });

  /*
   * restTypes will be a list of arrays,
   * each array will have
   * [
   *  "Types available in a particular group",
   *  "Display Name of a type",
   *  "Description of a type",
   *  "Category the particular type belongs to"
   * ]
   * */
  aggCategory.forEach(category => {
    const categoryRow = restTypes.filter(r => r[3] === category);
    if (categoryRow.length > 0) {
      const { typInfo, typValueMap } = getDataTypeInfo(
        categoryRow[0],
        pgCategoryCode[category],
        identifier,
        columnTypeValueMap
      );
      columnTypeValueMap = { ...columnTypeValueMap, ...typValueMap };
      columnDataTypes.push({
        label: pgCategoryCode[category],
        options: typInfo,
      });
    }
  });
  return {
    columnTypeValueMap,
    columnDataTypes,
  };
};

const getPlaceholder = column => {
  switch (column.type) {
    case 'timestamptz':
      return 'example: now()';
    case 'date':
      return '';
    case 'uuid':
      return 'example: gen_random_uuid()';
    default:
      return 'default_value';
  }
};

const getDefaultValue = column => {
  return ('default' in column && column.default.value) || '';
};

const getRecommendedTypeCasts = (dataType, typeCasts) => {
  return (dataType in typeCasts && typeCasts[dataType][3].split(',')) || [];
};

const inferDefaultValues = (defaultFuncs, typeCasts) => {
  let defaultValues = [];
  const visitedType = {};
  /* Current type is the type for which default values needs to be computed
   * Algorithm:
   *  Look for the types which the current type can be casted to
   *  Try to find the default values for the right type and accumulate it to an array
   * */
  const computeDefaultValues = currentType => {
    visitedType[currentType] = true;
    /* Retrieve the recommended type casts for the current type */
    const validRightCasts = getRecommendedTypeCasts(currentType, typeCasts);
    validRightCasts.forEach(v => {
      if (!visitedType[v]) {
        if (v in defaultFuncs) {
          visitedType[v] = true;
          defaultValues = [...defaultValues, ...defaultFuncs[v]];
        }
      }
    });
    return defaultValues;
  };
  return computeDefaultValues;
};

export {
  getDataOptions,
  getPlaceholder,
  getDefaultValue,
  getDataTypeInfo,
  getAllDataTypeMap,
  getDefaultFunctionsOptions,
  inferDefaultValues,
};
