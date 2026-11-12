import { createContext } from 'react';
import { MetadataTable } from '@hasura/shared/types';

type CurrentTableState = {
  tableType?: string;
  metadataTable: MetadataTable;
};

const defaultState: CurrentTableState = {
  metadataTable: {} as MetadataTable,
};

export const CurrentTableContext = createContext(defaultState);
