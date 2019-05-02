const defaultState = {
  inconsistentObjects: [],
  ongoingRequest: false,
  whitelistQueries: [
    '{\n  Album {\n    Title\n  }\n}',
    '{\n  article {\n    id\n    title\n    content\n    author {\n      id\n      name\n    }\n  }\n}',
    '{\n  Album {\n    Title\n  }\n}',
    '{\n  article {\n    id\n    title\n    content\n    author {\n      id\n      name\n    }\n  }\n}',
    '{\n  article {\n    id\n    title\n    content\n    author {\n      id\n      name\n    }\n  }\n}',
  ], // TODO: remove sample queries
};

export default defaultState;
