import { TypedUseSelectorHook, useDispatch, useSelector } from 'react-redux';
import { routerMiddleware } from 'react-router-redux';
import { browserHistory } from 'react-router';

// Since we only use it in dev, this warning doesn't make sense.
// eslint-disable-next-line import/no-extraneous-dependencies
import { configureStore } from '@reduxjs/toolkit';
import { telemetryMiddleware } from './telemetry';
import reducer from './reducer';

export const store = configureStore({
  reducer,
  middleware: getDefaultMiddleware => {
    // Middleware by default : https://redux-toolkit.js.org/api/getDefaultMiddleware#included-default-middleware
    return getDefaultMiddleware({
      // Removed because we use callbacks in some places and they are not serializable.
      // see https://redux.js.org/style-guide/style-guide#do-not-put-non-serializable-values-in-state-or-actions
      serializableCheck: false,
      // Remove this line when you want to
      // See https://redux.js.org/style-guide/style-guide#do-not-mutate-state
      immutableCheck: false,
    }).concat([routerMiddleware(browserHistory), telemetryMiddleware]);
  },

  // Enable redux browser devtools
  devTools: __DEVELOPMENT__,
});

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;

export const useAppDispatch = () => useDispatch<AppDispatch>();
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector;
export type AsyncThunkConfig = {
  dispatch: AppDispatch;
  state: RootState;
};
