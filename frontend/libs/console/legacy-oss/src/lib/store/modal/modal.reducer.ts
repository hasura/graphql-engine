import { ModalActions } from './modal.actions';

export type ModalStore = {
  modalKey: string | null;
};

const defaultState: ModalStore = {
  modalKey: null,
};

export const modalReducer = (
  state = defaultState,
  action: ModalActions
): ModalStore => {
  switch (action.type) {
    case 'Modal/SHOW':
      return {
        ...state,
        modalKey: action.data.modalKey,
      };
    case 'Modal/HIDE':
      return {
        ...state,
        modalKey: null,
      };
    default:
      return state;
  }
};
