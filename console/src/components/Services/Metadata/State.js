const defaultState = {
  inconsistentObjects: [],
  ongoingRequest: false,
  whitelistQueries: [
    { name: 'get_album', query: '{\n  Album {\n    Title\n  }\n}' },
    {
      name: 'get_article',
      query:
        '{\n  article {\n    id\n    title\n    content\n    author {\n      id\n      name\n    }\n  }\n}',
    },
    { name: 'get_album1', query: '{\n  Album {\n    Title\n  }\n}' },
    {
      name: 'get_article1',
      query:
        '{\n  article {\n    id\n    title\n    content\n    author {\n      id\n      name\n    }\n  }\n}',
    },
    {
      name: 'get_article2',
      query:
        '{\n  article {\n    id\n    title\n    content\n    author {\n      id\n      name\n    }\n  }\n}',
    },
  ], // TODO: remove sample queries
};

export default defaultState;
