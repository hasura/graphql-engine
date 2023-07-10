import { compose, createStore, applyMiddleware } from 'redux';
import { routerMiddleware } from 'react-router-redux';
import { browserHistory } from 'react-router';
import thunk from 'redux-thunk';
import { reduxStoreListener } from '@hasura/console-legacy-ce';

import reducer from './reducer';

let _finalCreateStore;

if (__DEVELOPMENT__) {
  const tools = [
    applyMiddleware(thunk, routerMiddleware(browserHistory)),
    require('redux-devtools').persistState(
      window.location.href.match(/[?&]debug_session=([^&]+)\b/)
    ),
  ];
  if (typeof window === 'object' && window.__REDUX_DEVTOOLS_EXTENSION__) {
    tools.push(window.__REDUX_DEVTOOLS_EXTENSION__());
  }
  _finalCreateStore = compose(...tools)(createStore);
} else {
  _finalCreateStore = compose(
    applyMiddleware(thunk, routerMiddleware(browserHistory))
  )(createStore);
}

const store = _finalCreateStore(reducer);

reduxStoreListener(store);

export default store;
