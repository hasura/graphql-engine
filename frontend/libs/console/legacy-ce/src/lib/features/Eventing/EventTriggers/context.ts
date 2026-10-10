import { createContext, useContext } from 'react';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { EventTrigger, Source, Table } from '@hasura/shared/types';

type EventTriggerWithTableInfo = MetadataSelectors.EventTriggerWithTableInfo;

export type EventTriggerDetailState = {
  currentTable: Table;
  eventTrigger: EventTrigger;
  currentSource: Source;
};

const defaultState: EventTriggerDetailState = {
  eventTrigger: {} as EventTriggerWithTableInfo,
  currentTable: {} as Table,
  currentSource: {} as Source,
};

export const EventTriggerDetailContext = createContext(defaultState);

export const useEventTriggerDetailContext = () =>
  useContext(EventTriggerDetailContext);
