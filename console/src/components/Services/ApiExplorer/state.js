import globals from '../../../Globals';

const defaultHeader = [
  {
    key: 'content-type',
    value: 'application/json',
    isActive: true,
    isNewHeader: false,
    isDisabled: true,
  },
];
defaultHeader.push({
  key: '',
  value: '',
  isActive: false,
  isNewHeader: true,
});

const getUrl = path => {
  return `${globals.dataApiUrl}${path}`;
};

const dataApisContent = [];
// check project version
dataApisContent.push({
  id: 'DataApi-3',
  details: {
    title: 'GraphQL API',
    description:
      'GraphQL API for CRUD operations on tables & views in your database',
    category: 'data',
  },
  request: {
    method: 'POST',
    url: getUrl('/v1/graphql'),
    headers: defaultHeader,
    bodyType: 'graphql',
    params: JSON.stringify({}, null, 4),
  },
});

const dataApis = {
  title: 'Data',
  content: dataApisContent,
};

const explorerData = {
  sendingRequest: false,
  enableResponseSection: false,
  response: {},
  fileObj: null,
};

const defaultApi = dataApis.content[0];

const defaultState = {
  currentTab: 0,
  displayedApi: defaultApi,
  modalState: {
    isOpen: false,
    isCopied: false,
  },
  explorerData,
  authApiExpanded: 'Username-password Login',
  headerFocus: false,
};

export default defaultState;

export { defaultApi, defaultHeader };
