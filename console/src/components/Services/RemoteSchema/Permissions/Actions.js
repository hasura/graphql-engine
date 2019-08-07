import { permissionState } from '../state';

const reducer = (state = permissionState, action) => {
  switch (action.type) {
    default:
      return {
        ...state,
      };
  }
};

export default reducer;
