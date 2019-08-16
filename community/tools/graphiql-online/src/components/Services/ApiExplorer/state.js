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

const dataApisContent = [];
dataApisContent.push({
  id: 'DataApi',
  details: {
    title: '',
    description:
      'Explore GraphQL APIs with headers',
    category: 'data',
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
  headerFocus: false,
  graphqlEndpoint: '',
};

export default defaultState;

export { defaultApi, defaultHeader };
