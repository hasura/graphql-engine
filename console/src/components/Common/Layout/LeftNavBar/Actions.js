/* State

{
  ongoingRequest : false, //true if request is going on
  lastError : null OR <string>
  lastSuccess: null OR <string>
}

*/
import defaultState from './State';

const SET_USERNAME = 'PageContainer/SET_USERNAME';

// HTML Component defines what state it needs
// HTML Component should be able to emit actions
// When an action happens, the state is modified (using the reducer function)
// When the state is modified, anybody dependent on the state is asked to update
// HTML Component is listening to state, hence re-renders

const homeReducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_USERNAME:
      return { username: action.data };
    default:
      return state;
  }
};

const setUsername = username => ({ type: SET_USERNAME, data: username });

export default homeReducer;
export { setUsername };
