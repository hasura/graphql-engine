import { MAKE_REQUEST, REQUEST_SUCCESS, REQUEST_ERROR } from '../EventActions';

const modifyReducer = (tableName, schemas, modifyStateOrig, action) => {
  const modifyState = JSON.parse(JSON.stringify(modifyStateOrig));

  switch (action.type) {
    case MAKE_REQUEST:
      return {
        ...modifyState,
        ongoingRequest: true,
        lastError: null,
        lastSuccess: null,
      };
    case REQUEST_SUCCESS:
      return {
        ...modifyState,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: true,
      };
    case REQUEST_ERROR:
      return {
        ...modifyState,
        ongoingRequest: false,
        lastError: action.data,
        lastSuccess: false,
      };
    default:
      return modifyState;
  }
};

export default modifyReducer;
