import React from 'react';
import { RoleLimits, RoleState } from './utils';
import styles from './Security.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import { isEmpty } from '../../../Common/utils/jsUtils';

type Limit =
  | number
  | {
      unique_params: 'IP' | string[];
      max_reqs_per_min: number;
    };

interface LimitFormProps {
  limit: keyof RoleLimits;
  label: {
    title: string;
    info: string;
  };
  state: RoleState;
  roleLimit: Limit;
  globalLimit: Limit;
  unique_params_global: 'IP' | string[];
  unique_params_role: 'IP' | string[];
  max_reqs_global: number;
  max_reqs_role: number;
  role: string;
  onRadioChange: (limit: keyof RoleLimits, state: RoleState) => () => void;
  onUniqueParamsChange: (role: string) => (val: string) => void;
  onInputChange: (
    limit: keyof RoleLimits,
    role: string
  ) => (val: string) => void;
}

export const LimitsForm: React.FC<LimitFormProps> = ({
  limit,
  label,
  state,
  roleLimit,
  globalLimit,
  unique_params_global,
  unique_params_role,
  max_reqs_global,
  max_reqs_role,
  role,
  onRadioChange,
  onUniqueParamsChange,
  onInputChange,
}) => {
  const isRateLimit = limit === 'rate_limit';
  const isGlobal = role === 'global';
  const isdisabledGlobally =
    (isEmpty(globalLimit) || globalLimit < 0) && !isGlobal;

  const renderValue = () => {
    const rateValue = isGlobal ? max_reqs_global : max_reqs_role;
    const othervalue = isGlobal
      ? globalLimit >= 0
        ? (globalLimit as number)
        : ''
      : (roleLimit as number);
    return isRateLimit ? rateValue ?? '' : othervalue ?? '';
  };
  const renderUniqueParamValue = () => {
    const unique_params = isGlobal ? unique_params_global : unique_params_role;
    return unique_params === 'IP' ? 'IP' : unique_params?.join(', ') ?? '';
  };
  return (
    <div key={limit} className={styles.form_group}>
      <div className={styles.form_container}>
        <div className={styles.left}>
          <h5>{label.title}</h5>
          <p>{label.info}</p>
        </div>
        <div className={styles.right}>
          <div className={styles.radio_group}>
            <div className="radio_input">
              <input
                type="radio"
                id={`enable_${limit}`}
                checked={state === RoleState.enabled}
                onChange={onRadioChange(limit, RoleState.enabled)}
              />
              <label htmlFor={`enable_${limit}`}>
                {!isGlobal ? 'Custom' : 'Enable'}
              </label>
            </div>
            {isGlobal && (
              <div className="radio_input">
                <input
                  type="radio"
                  id={`disable_${limit}`}
                  checked={state === RoleState.disabled}
                  onChange={onRadioChange(limit, RoleState.disabled)}
                />
                <label htmlFor={`disable_${limit}`}>Disable</label>
              </div>
            )}
            {!isGlobal && (
              <div className="radio_input">
                <input
                  type="radio"
                  id={`global_${limit}`}
                  checked={state === RoleState.global}
                  onChange={onRadioChange(limit, RoleState.global)}
                />
                <label htmlFor={`global_${limit}`}>Global</label>
              </div>
            )}
          </div>
          <>
            <input
              type="number"
              min={0}
              className={`form-control ${styles.special_input}`}
              value={renderValue()}
              onChange={e => onInputChange(limit, role)(e.target.value)}
              placeholder={isRateLimit ? 'Request Per Minute' : 'Limit'}
              disabled={
                state === RoleState.global || state === RoleState.disabled
              }
            />

            {isRateLimit && (
              <div className={styles.unique_params}>
                <div className={styles.radio_group}>
                  <b>Unique Parameter</b>
                  <div className="radio_input">
                    <input
                      type="radio"
                      id="ip_address"
                      checked={
                        isGlobal
                          ? unique_params_global === 'IP'
                          : unique_params_role === 'IP'
                      }
                      disabled={
                        state === RoleState.global ||
                        state === RoleState.disabled
                      }
                      onChange={() => onUniqueParamsChange(role)('IP')}
                    />
                    <label htmlFor="ip_address">IP Address</label>
                  </div>
                  <div className="radio_input">
                    <input
                      type="radio"
                      id="session_variable"
                      checked={
                        isGlobal
                          ? unique_params_global !== 'IP'
                          : unique_params_role !== 'IP'
                      }
                      disabled={
                        state === RoleState.global ||
                        state === RoleState.disabled
                      }
                      onChange={() => onUniqueParamsChange(role)('')}
                    />
                    <label htmlFor="session_variable">
                      Session Variable(s){' '}
                      <ToolTip message="multiple session variables can be provided as comma separated values" />
                    </label>
                  </div>
                </div>
                {((isGlobal && unique_params_global !== 'IP') ||
                  (!isGlobal && unique_params_role !== 'IP')) && (
                  <input
                    type="text"
                    className={`form-control ${styles.special_input}`}
                    value={renderUniqueParamValue()}
                    disabled={
                      state === RoleState.global || state === RoleState.disabled
                    }
                    placeholder="x-hasura-user-id, x-hasura-org"
                    onChange={e => onUniqueParamsChange(role)(e.target.value)}
                  />
                )}
              </div>
            )}
          </>
        </div>
        {isdisabledGlobally && <div className={styles.disabled_cover} />}
      </div>
      {isdisabledGlobally && (
        <p className={styles.disabled_text}>
          Global Setting for{' '}
          <b>
            <i>{label.title?.split('(')[0]}</i>
          </b>{' '}
          needs to be set first
        </p>
      )}
    </div>
  );
};
