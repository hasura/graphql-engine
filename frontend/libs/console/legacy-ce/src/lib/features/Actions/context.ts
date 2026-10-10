import { createContext, useContext } from 'react';
import { Action, Metadata } from '@hasura/shared/types';

export type CurrentActionState<A = Action | undefined> = Metadata & {
  currentAction: A;
};

const defaultState: CurrentActionState<Action | undefined> = {
  resource_version: 0,
  metadata: {} as Metadata['metadata'],
  currentAction: undefined,
};

export const CurrentActionContext = createContext(defaultState);

export const useCurrentActionContext = () =>
  useContext(CurrentActionContext) as CurrentActionState<Action>;
