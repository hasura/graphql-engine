import React, { useEffect } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { Button } from '../../../Common';
import { TableFormProps } from '../../../Common/Table';
import { RoleLimits, RoleState } from './utils';
import styles from './Security.scss';
import { removeAPILimits, updateAPILimits } from './actions';
import { apiLimitActions, ApiLimitsFormSate } from './state';
import { isEmpty } from '../../../Common/utils/jsUtils';
import { Dispatch } from '../../../../types';
import { LimitsForm } from './LimitsForm';

export const labels: Record<
  keyof RoleLimits,
  { title: string; info: string }
> = {
  depth_limit: {
    title: 'Depth Limit',
    info: 'Set the maximum relation depth a request can traverse.',
  },
  node_limit: {
    title: 'Node Limit',
    info:
      'Set the maximum number of nodes which can be requested in a request.',
  },
  rate_limit: {
    title: 'Request Rate Limit (Requests Per Minute)',
    info:
      'Set a request rate limit for this role. You can also combine additional unique parameters for more granularity.',
  },
};

interface LimitsFormWrapperProps extends TableFormProps<RoleLimits> {
  disabled: boolean;
  setLoading: React.Dispatch<React.SetStateAction<boolean>>;
  dispatch: Dispatch;
}

const LimitsFormWrapper: React.FC<LimitsFormWrapperProps> = ({
  collapseForm,
  currentData,
  currentRowKey,
  disabled,
  setLoading,
  dispatch: parentDispatch,
}) => {
  const api_limits = useSelector((state: ApiLimitsFormSate) => state);
  const rateLimit = useSelector((state: ApiLimitsFormSate) => state.rate_limit);
  const nodeLimit = useSelector((state: ApiLimitsFormSate) => state.node_limit);
  const depthLimit = useSelector(
    (state: ApiLimitsFormSate) => state.depth_limit
  );
  const dispatch = useDispatch();
  useEffect(() => {
    dispatch(apiLimitActions.resetTo(currentData));
  }, [currentData, disabled, dispatch]);

  useEffect(() => {
    dispatch(apiLimitActions.setDisable(disabled));
  }, [disabled, dispatch]);

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setLoading(true);
    parentDispatch(
      updateAPILimits({
        api_limits,
        successCb() {
          collapseForm();
          setLoading(false);
        },
        errorCb() {
          setLoading(false);
        },
      })
    );
  };

  const isDisabled = (currentRole: string) => {
    if (!currentRole) return true;
    const disabled_for_role =
      isEmpty(depthLimit?.global) &&
      isEmpty(nodeLimit?.global) &&
      isEmpty(rateLimit?.global);
    return currentRole !== 'global' && disabled_for_role;
  };

  const onUniqueParamsChange = (role: string) => (val: string) => {
    const value = val === 'IP' ? val : val.split(/,\s*/);
    if (role === 'global') {
      return dispatch(apiLimitActions.updateGlobalUniqueParams(value));
    }
    return dispatch(apiLimitActions.updateUniqueParams({ limit: value, role }));
  };

  const onRadioChange = (limit: keyof RoleLimits, state: RoleState) => () => {
    switch (limit) {
      case 'depth_limit':
        return dispatch(apiLimitActions.updateDepthLimitState(state));
      case 'node_limit':
        return dispatch(apiLimitActions.updateNodeLimitState(state));
      default:
        return dispatch(apiLimitActions.updateRateLimitState(state));
    }
  };
  const onInputChange = (limit: keyof RoleLimits, role: string) => (
    val: string
  ) => {
    const value = parseInt(val, 10);
    if (Number.isNaN(value)) return;
    if (role !== 'global') {
      switch (limit) {
        case 'depth_limit':
          return dispatch(
            apiLimitActions.updateDepthLimitRole({ role, limit: value })
          );
        case 'node_limit':
          return dispatch(
            apiLimitActions.updateNodeLimitRole({ role, limit: value })
          );
        default:
          return dispatch(
            apiLimitActions.updateMaxReqPerMin({ role, limit: value })
          );
      }
    }
    switch (limit) {
      case 'depth_limit':
        return dispatch(apiLimitActions.updateGlobalDepthLimit(value));
      case 'node_limit':
        return dispatch(apiLimitActions.updateGlobalNodeLimit(value));
      default:
        return dispatch(apiLimitActions.updateGlobalMaxReqPerMin(value));
    }
  };

  const removeBtnClickHandler = (role: string) => {
    parentDispatch(
      removeAPILimits({
        role,
        callback: collapseForm,
      })
    );
  };

  return (
    <section className={styles.form_section}>
      <div className={styles.top}>
        <Button size="xs" onClick={() => collapseForm()}>
          Close
        </Button>
        <h5>
          {currentRowKey === 'global'
            ? 'Global Settings'
            : `Role: ${currentRowKey}`}
        </h5>
      </div>
      <form onSubmit={submit}>
        {Object.entries(labels).map(([key, label]) => {
          const limit = key as keyof RoleLimits;
          const role = currentRowKey;
          return (
            <LimitsForm
              limit={limit}
              label={label}
              role={role}
              state={api_limits[limit]?.state ?? RoleState.global}
              globalLimit={api_limits[limit]?.global}
              roleLimit={api_limits[limit]?.per_role?.[role]}
              unique_params_global={rateLimit?.global?.unique_params}
              unique_params_role={rateLimit?.per_role?.[role]?.unique_params}
              max_reqs_global={rateLimit?.global.max_reqs_per_min}
              max_reqs_role={rateLimit?.per_role?.[role]?.max_reqs_per_min}
              onInputChange={onInputChange}
              onRadioChange={onRadioChange}
              onUniqueParamsChange={onUniqueParamsChange}
            />
          );
        })}
        <div className={styles.form_footer}>
          <div className="submit_btn">
            <Button
              size="xs"
              color="yellow"
              type="submit"
              disabled={isDisabled(currentRowKey)}
            >
              Save Settings
            </Button>
          </div>
          <div className={styles.padding_left_sm}>
            <Button
              size="sm"
              color="red"
              disabled={isDisabled(currentRowKey)}
              onClick={() => removeBtnClickHandler(currentRowKey)}
            >
              Remove Settings
            </Button>
          </div>
        </div>
      </form>
    </section>
  );
};

export default LimitsFormWrapper;
