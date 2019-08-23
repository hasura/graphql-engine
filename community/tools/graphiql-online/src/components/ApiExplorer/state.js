const existingHeaders = window.__env.headers;

const defaultHeader = [
  {
    key: 'Content-Type',
    value: 'application/json',
    isActive: true,
    isNewHeader: false,
    isDisabled: true,
  },
];

if (existingHeaders) {
  Object.keys(existingHeaders).forEach((key) => {
    defaultHeader.push({
      key,
      value: existingHeaders[key],
      isActive: true,
      isNewHeader: false,
    })
  });
}
defaultHeader.push({
  key: '',
  value: '',
  isActive: false,
  isNewHeader: true,
});

const getUrl = path => {
  return `${window.__env.graphqlEndpoint}`;
};

const dataApisContent = [];
// check project version
dataApisContent.push({
  id: 'DataApi-3',
  details: {
    title: '',
    description:
      'Explore GraphQL APIs with headers',
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
  graphqlEndpoint: ''
};

export default defaultState;

export { defaultApi, defaultHeader };
