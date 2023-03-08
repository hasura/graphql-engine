const defaultState = {
  oAuthResponse: {},
  projectId: null,
  projectName: null,
  personalAccessToken: null,
  metadata: {
    sources: [],
    api_limits: {},
    allowlist: null,
    query_collections: null,
  },
  project: {
    id: '',
    name: '',
    privileges: [],
    metricsFQDN: '',
  },
  projectEntitlements: [],
};

export default defaultState;
