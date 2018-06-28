import Globals from 'Globals';

const defaultHeader = [
  {
    key: 'Content-Type',
    value: 'application/json',
    isActive: true,
    isNewHeader: false,
  },
  {
    key: '',
    value: '',
    isActive: false,
    isNewHeader: true,
  },
];
const incompleteAuthHeader = [
  {
    key: 'Content-Type',
    value: 'application/json',
    isActive: true,
    isNewHeader: false,
  },
  {
    key: 'Authorization',
    value: 'Bearer <auth-token>',
    isActive: false,
    isNewHeader: false,
  },
  {
    key: '',
    value: '',
    isActive: false,
    isNewHeader: true,
  },
];

const getUrl = path => {
  return `${Globals.dataApiUrl}${path}`;
};

const dataApisContent = [];
// check project version
dataApisContent.push({
  id: 'DataApi-3',
  details: {
    title: 'GraphQL API',
    description:
      'Hasura provides GraphQL APIs which can be used to perform CRUD operations on the tables that you create',
    category: 'data',
  },
  request: {
    method: 'POST',
    url: getUrl('/v1alpha1/graphql'),
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
};

export default defaultState;

export { defaultApi, defaultHeader, incompleteAuthHeader };
