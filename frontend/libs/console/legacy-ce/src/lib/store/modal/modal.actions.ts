export type ShowModalAction = {
  type: 'Modal/SHOW';
  data: {
    modalKey: string;
  };
};

export type HideModalAction = {
  type: 'Modal/HIDE';
};

export type ModalActions = ShowModalAction | HideModalAction;

export const showModal = (modalKey: string) => {
  const action: ShowModalAction = {
    type: 'Modal/SHOW',
    data: {
      modalKey,
    },
  };
  return action;
};

export const hideModal = () => {
  const action: HideModalAction = {
    type: 'Modal/HIDE',
  };
  return action;
};
