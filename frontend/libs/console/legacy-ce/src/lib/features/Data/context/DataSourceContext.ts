import { createContext, useContext } from 'react';
import type { Metadata, Source } from '@hasura/shared/types';

export type DataSourceState = Metadata & {
  currentSource: Source;
};

const defaultState: DataSourceState = {
  resource_version: 0,
  metadata: {} as Metadata['metadata'],
  currentSource: {} as Source,
};

export const DataSourceContext = createContext(defaultState);

export const useDataSourceContext = () => useContext(DataSourceContext);
