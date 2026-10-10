import { createContext, useContext } from 'react';
import { MetadataTable } from '@hasura/shared/types';

export type CurrentTableState = {
  tableType?: string;
  metadataTable: MetadataTable;
};

const defaultState: CurrentTableState = {
  metadataTable: {} as MetadataTable,
};

export const CurrentTableContext = createContext(defaultState);

export const useCurrentTableContext = () => useContext(CurrentTableContext);
