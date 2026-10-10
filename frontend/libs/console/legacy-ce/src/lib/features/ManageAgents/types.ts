import { DataConnectorUri } from '@hasura/shared/types';

export type DcAgent = {
  name: string;
  uri: DataConnectorUri;
};
