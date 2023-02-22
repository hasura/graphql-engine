const defaultState = {
  sql: '',
  ongoingRequest: false,
  lastError: null,
  lastSuccess: null,
  resultType: null,
  result: [],
  resultHeaders: [],
  isModalOpen: false,
  isCascadeChecked: false,
  isReadOnlyChecked: false,
  isMigrationChecked: false,
  isTableTrackChecked: false,
  showTrackTable: false,
};

export default defaultState;
