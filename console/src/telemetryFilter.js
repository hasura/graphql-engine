const filterEventsBlockList = [
  'App/ONGOING_REQUEST',
  'App/DONE_REQUEST',
  'App/FAILED_REQUEST',
  'App/ERROR_REQUEST',
];

const filterPayloadAllowList = [
  'ViewTable/FilterQuery/SET_FILTEROP',
  'ViewTable/FilterQuery/SET_ORDERTYPE',
  'Login/REQUEST_SUCCESS',
];

export { filterEventsBlockList, filterPayloadAllowList };
