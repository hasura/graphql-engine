const filterEventsBlockList = [
  'App/ONGOING_REQUEST',
  'App/DONE_REQUEST',
  'App/FAILED_REQUEST',
  'App/ERROR_REQUEST',
  'DataManage/SET_DEPLOYMENT',
  'Main/SET_LIMITS',
  'DataManage/SET_DEP_POLL',
  'DataManage/SET_DEFAULTS',
  'Home/GET_LATEST_PLATFORM_VERSION_SUCCESS',
];

const filterPayloadAllowList = [
  'Home/LOAD_PLATFORM_ERROR',
  'ViewTable/FilterQuery/SET_FILTEROP',
  'ViewTable/FilterQuery/SET_ORDERTYPE',
];

export { filterEventsBlockList, filterPayloadAllowList };
