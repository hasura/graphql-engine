import { getLocalStorageItem, RAW_SQL_STATEMENT_TIMEOUT } from '../../../Common/utils/localStorageUtils';

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
  isMigrationChecked: false,
  isTableTrackChecked: false,
  showTrackTable: false,
  statementTimeout: Number(getLocalStorageItem(RAW_SQL_STATEMENT_TIMEOUT)) || null,
};

export default defaultState;
