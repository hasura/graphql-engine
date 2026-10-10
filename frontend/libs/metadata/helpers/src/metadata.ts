import { Metadata } from '@hasura/shared/types';

export const isMetadataEmpty = (metadataObject: Metadata['metadata']) => {
  const { actions, sources, remote_schemas } = metadataObject;
  const hasRemoteSchema = remote_schemas && remote_schemas.length;
  const hasAction = actions && actions.length;
  const hasTable = sources.some((source) => source.tables.length);
  return !(hasRemoteSchema || hasAction || hasTable);
};
