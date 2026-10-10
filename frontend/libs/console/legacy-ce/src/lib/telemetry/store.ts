import { create } from 'zustand';
import {
  ConsoleNotification,
  CatalogState,
  ConsoleState,
} from '@hasura/metadata/api';

export type TelemetryState = {
  consoleState: ConsoleState | null;
  hasuraUuid: string;
  notifications: ConsoleNotification[];
  setCatalogState: (opts: CatalogState) => void;
  setNotifications: (notifications: ConsoleNotification[]) => void;
};

export const useTelemetryStore = create<TelemetryState>((set) => ({
  consoleState: null,
  hasuraUuid: '',
  notifications: [],
  setCatalogState: (opts: CatalogState) =>
    set({ consoleState: opts.console_state, hasuraUuid: opts.id }),
  setNotifications: (notifications: ConsoleNotification[]) =>
    set({ notifications }),
}));
