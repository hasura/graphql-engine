const defaultState = {
  sql: '',
  ongoingRequest: false,
  lastError: null,
  lastSuccess: null,

  resultType: null,
  result: [],
  resultHeaders: [],
  isModalOpen: false,
  isMigrationChecked: false,
  isTableTrackChecked: false,
  showTrackTable: false,
};

export default defaultState;
