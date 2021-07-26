import { createSlice, PayloadAction, configureStore } from '@reduxjs/toolkit';
import { Nullable } from './../../../Common/utils/tsUtils';
import { RoleState } from './utils';

const initialState = {
  disabled: true,
  depth_limit: {
    global: -1,
    state: RoleState.disabled,
    per_role: {} as Record<string, number>,
  },
  node_limit: {
    global: -1,
    state: RoleState.disabled,
    per_role: {} as Record<string, number>,
  },
  rate_limit: {
    global: {} as {
      unique_params: Nullable<'IP' | string[]>;
      max_reqs_per_min: number;
    },
    state: RoleState.disabled,
    per_role: {} as Record<
      string,
      { unique_params: Nullable<'IP' | string[]>; max_reqs_per_min: number }
    >,
  },
};

type LimitPayload<T = number> = {
  role: string;
  limit: T;
};

const formStateSLice = createSlice({
  name: 'api_limits_form',
  initialState,
  reducers: {
    updateGlobalDepthLimit(state, action: PayloadAction<number>) {
      state.depth_limit.global = action.payload;
    },
    updateDepthLimitRole(state, action: PayloadAction<LimitPayload>) {
      state.depth_limit.per_role[action.payload.role] = action.payload.limit;
    },
    updateDepthLimitState(state, action: PayloadAction<RoleState>) {
      state.depth_limit.state = action.payload;
    },
    updateGlobalNodeLimit(state, action: PayloadAction<number>) {
      state.node_limit.global = action.payload;
    },
    updateNodeLimitRole(state, action: PayloadAction<LimitPayload>) {
      state.node_limit.per_role[action.payload.role] = action.payload.limit;
    },
    updateNodeLimitState(state, action: PayloadAction<RoleState>) {
      state.node_limit.state = action.payload;
    },
    updateUniqueParams(
      state,
      action: PayloadAction<LimitPayload<'IP' | string[]>>
    ) {
      if (!state.rate_limit.per_role[action.payload.role]) {
        state.rate_limit.per_role[action.payload.role] = {
          unique_params: action.payload.limit,
          max_reqs_per_min: 1,
        };
      } else {
        state.rate_limit.per_role[action.payload.role].unique_params =
          action.payload.limit;
      }
    },
    updateMaxReqPerMin(state, action: PayloadAction<LimitPayload>) {
      if (!state.rate_limit.per_role[action.payload.role]) {
        state.rate_limit.per_role[action.payload.role] = {
          max_reqs_per_min: action.payload.limit,
          unique_params: null,
        };
      } else {
        state.rate_limit.per_role[action.payload.role].max_reqs_per_min =
          action.payload.limit;
      }
    },
    updateGlobalUniqueParams(state, action: PayloadAction<'IP' | string[]>) {
      state.rate_limit.global.unique_params = action.payload;
    },
    updateGlobalMaxReqPerMin(state, action: PayloadAction<number>) {
      state.rate_limit.global.max_reqs_per_min = action.payload;
    },
    updateRateLimitState(state, action: PayloadAction<RoleState>) {
      state.rate_limit.state = action.payload;
    },
    resetTo(
      state,
      action: PayloadAction<Omit<typeof initialState, 'disabled'>>
    ) {
      state.depth_limit = action.payload.depth_limit;
      state.node_limit = action.payload.node_limit;
      state.rate_limit = action.payload.rate_limit;
    },
    setDisable(state, action: PayloadAction<boolean>) {
      state.disabled = action.payload;
    },
  },
});

export const apiLimitStore = configureStore({
  reducer: formStateSLice.reducer,
  devTools: false,
});

export type ApiLimitsFormSate = ReturnType<typeof apiLimitStore.getState>;

export const apiLimitActions = formStateSLice.actions;
