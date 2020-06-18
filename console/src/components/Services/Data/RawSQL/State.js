import {
  getLocalStorageItem,
  LS_RAW_SQL_STATEMENT_TIMEOUT,
} from '../../../Common/utils/localStorageUtils';

const persistedStatementTimeout = Number(
  getLocalStorageItem(LS_RAW_SQL_STATEMENT_TIMEOUT)
);
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
  statementTimeout: persistedStatementTimeout || 10,
};

export default defaultState;
