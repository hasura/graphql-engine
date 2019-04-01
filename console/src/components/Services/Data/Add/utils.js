import { aggCategory, pgCategoryCode } from '../Common/PgInfo';

const getDataTypeInfo = (row, categoryInfo, colId) => {
  const columnTypeValueMap = {};
  const typInfo = row[0].split(',');
  const typDisplayName = row[1].split(',');
  const typDescription = row[2].split(',');
  const currTypeObj = typInfo.map((t, i) => {
    const optObj = {
      value: t,
      label: typDisplayName[i],
      key: `${categoryInfo}_${i}`,
      colIdentifier: colId,
      description: typDescription[i],
    };
    columnTypeValueMap[t] = optObj;
    return optObj;
  });
  return { typInfo: currTypeObj, typValueMap: columnTypeValueMap };
};

const getDataOptions = (dataTypes, restTypes, i) => {
  let columnTypeValueMap = {};
  const columnDataTypes = [];
  const mainOpts = [];
  dataTypes.forEach((d, dKey) => {
    mainOpts.push({
      value: d.value,
      label: d.name,
      description: d.description,
      key: dKey,
      colIdentifier: i,
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
        i
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

export { getDataOptions };
