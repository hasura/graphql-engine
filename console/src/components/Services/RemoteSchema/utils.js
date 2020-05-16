import { FILTER_REMOTE_SCHEMAS } from './Actions';

export const getAllRemoteSchemas = getState => {
  return getState().remoteSchemas.listData.remoteSchemas;
};

export const filterItem = dispatch => {
  return (dataList, searchVal) => {
    // form new schema
    const matchedTables = dataList.filter(data => {
      return (
        data.name
          .toLowerCase()
          .indexOf(searchVal ? searchVal.toLowerCase() : '') !== -1
      );
    });
    dispatch({
      type: FILTER_REMOTE_SCHEMAS,
      data: {
        filtered: matchedTables,
        searchQuery: searchVal,
      },
    });
  };
};
