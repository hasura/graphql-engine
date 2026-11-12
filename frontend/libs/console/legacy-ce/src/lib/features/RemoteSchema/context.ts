import { createContext, useContext } from 'react';
import type { Metadata, RemoteSchema } from '@hasura/shared/types';

type CurrentRemoteSchemaState = Metadata & {
  currentRemoteSchema: RemoteSchema;
};

const defaultState: CurrentRemoteSchemaState = {
  resource_version: 0,
  metadata: {} as Metadata['metadata'],
  currentRemoteSchema: {} as RemoteSchema,
};

export const CurrentRemoteSchemaContext = createContext(defaultState);

export const useCurrentRemoteSchemaContext = () =>
  useContext(CurrentRemoteSchemaContext);
