import { createSelector } from 'reselect';
import { ModalStore } from './modal.reducer';

const getState = (state: any) => state.modal;

export const modalKeySelector = createSelector(
  getState,
  (modalState: ModalStore) => modalState?.modalKey
);
