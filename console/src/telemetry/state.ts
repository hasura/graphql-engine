import { Nullable } from '../components/Common/utils/tsUtils';
import { ConsoleState } from '../types';

const defaultTelemetryState: ConsoleState = {
  console_opts: null,
  hasura_uuid: '',
};

export default defaultTelemetryState;
