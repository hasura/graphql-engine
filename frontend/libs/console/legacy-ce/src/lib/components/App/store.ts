import { create } from 'zustand';

type State = {
  percent: number;
  requestStatus: 'success' | 'error' | 'connection-error' | 'ongoing' | 'none';
  reset: () => void;
  startRequest: () => void;
  updateProgress: (percent: number) => void;
  failedRequest: (reason: 'error' | 'connection-error') => void;
};

export const useGlobalLoadingStore = create<State>((set, get) => ({
  percent: 0,
  requestStatus: 'none',
  reset: () =>
    set({
      percent: 0,
      requestStatus: 'none',
    }),
  startRequest: () =>
    set({
      percent: 10,
      requestStatus: 'ongoing',
    }),
  updateProgress: (percent: number) =>
    set({
      percent,
      requestStatus: 'ongoing',
    }),
  failedRequest: (reason: 'error' | 'connection-error') =>
    set({
      percent: 0,
      requestStatus: reason,
    }),
}));
