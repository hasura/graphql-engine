import { DataConnectorUri } from '../hasura-metadata-types';

export type DcAgent = {
  name: string;
  uri: DataConnectorUri;
};
