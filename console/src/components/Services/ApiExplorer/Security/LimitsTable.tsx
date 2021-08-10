import React, { useState } from 'react';
import { HasuraMetadataV3 } from '../../../../metadata/types';
import { Dispatch } from '../../../../types';
import {
  Table,
  TableForm,
  TableHeader,
  TableLegend,
  TableRow,
  TableSideBar,
} from '../../../Common/Table';
import { updateAPILimits } from './actions';
import LimitsForm from './LimitsFormWrapper';
import styles from './Security.scss';

import {
  getLimitsforRole,
  getLimitsforUnknownRole,
  Legends,
  RoleLimits,
  RoleState,
} from './utils';

interface Props {
  headers: string[];
  keys: string[];
  roles: string[];
  metadata: HasuraMetadataV3 | null;
  dispatch: Dispatch;
}

const LimitsTable: React.FC<Props> = ({
  headers,
  keys,
  roles,
  metadata,
  dispatch: parentDispatch,
}) => {
  const [loading, setLoading] = useState(false);
  const getRowData = metadata ? getLimitsforRole(metadata) : null;
  const apiLimitsDisabled = metadata?.api_limits?.disabled ?? false;
  const updateGlobalAPISetting = (flag: boolean) => {
    parentDispatch(
      updateAPILimits({
        api_limits: {
          disabled: flag,
        },
      })
    );
  };

  if (getRowData && metadata)
    return (
      <>
        <div className={styles.enable_container}>
          <div className={styles.left}>
            <h5>Enable API Limits</h5>
            <p>Enable additional API limits.</p>
          </div>
          <div className={styles.right}>
            <div className={styles.radio_group}>
              <div className="radio_input">
                <input
                  type="radio"
                  id="enable"
                  className="legacy-input-fix"
                  checked={!apiLimitsDisabled}
                  disabled={loading}
                  onChange={() => updateGlobalAPISetting(false)}
                />
                <label htmlFor="enable">Enabled</label>
              </div>
              <div className="radio_input">
                <input
                  type="radio"
                  id="disable"
                  className="legacy-input-fix"
                  disabled={loading}
                  checked={apiLimitsDisabled}
                  onChange={() => updateGlobalAPISetting(true)}
                />
                <label htmlFor="disable">Disabled</label>
              </div>
            </div>
          </div>
        </div>
        <div className={styles.max_width_90}>
          <Table columnCount={keys.length} rowCount={roles.length + 3}>
            <TableHeader headers={headers} keys={keys} />
            <TableSideBar
              items={['admin', 'global', ...roles, '']}
              renderItem={item =>
                item === 'global' ? (
                  <div>
                    <i className={`${styles.margin_right} fa fa-globe`} />
                    Global
                  </div>
                ) : (
                  item
                )
              }
            />
            <TableRow
              index="admin"
              entries={['full access']}
              isSingleColumn
              readonly
              renderCol={({ data }) => data}
            />
            <TableRow
              index="global"
              entries={getRowData('global')}
              renderCol={({ data: { global, state } }) => {
                const unique_params =
                  typeof global !== 'number' ? global?.unique_params : null;
                return (
                  <div>
                    <span>
                      {state === RoleState.enabled ? (
                        <Legends.Enabled />
                      ) : (
                        <Legends.Disabled />
                      )}
                    </span>
                    {state === RoleState.enabled
                      ? typeof global === 'number'
                        ? global
                        : global?.max_reqs_per_min
                      : null}
                    {unique_params
                      ? ` (on ${
                          unique_params === 'IP'
                            ? 'IP Address'
                            : unique_params.join(', ')
                        })`
                      : null}
                  </div>
                );
              }}
            />
            {roles.map(role => (
              <TableRow
                index={role}
                entries={getRowData(role)}
                renderCol={({ data: { per_role, state } }) => {
                  const roleLimit = per_role?.[role];
                  const max_reqs_per_min =
                    typeof roleLimit !== 'number'
                      ? roleLimit?.max_reqs_per_min
                      : null;
                  const unique_params =
                    typeof roleLimit !== 'number'
                      ? roleLimit?.unique_params
                      : null;
                  return (
                    <div>
                      <span>
                        {state === RoleState.global ? (
                          <Legends.Global />
                        ) : state === RoleState.enabled ? (
                          <Legends.Enabled />
                        ) : (
                          <Legends.Disabled />
                        )}
                      </span>
                      {state === RoleState.enabled
                        ? typeof roleLimit === 'number'
                          ? roleLimit
                          : max_reqs_per_min
                        : null}
                      {unique_params
                        ? ` (on ${
                            unique_params === 'IP'
                              ? 'IP Address'
                              : unique_params.join(', ')
                          })`
                        : null}
                    </div>
                  );
                }}
              />
            ))}
            <TableRow
              index=""
              entries={getLimitsforUnknownRole(metadata)}
              renderCol={({ data: { state } }) => (
                <>
                  {state === RoleState.global ? (
                    <Legends.Global />
                  ) : (
                    <Legends.Disabled />
                  )}
                </>
              )}
            />

            <TableForm<RoleLimits>>
              {props => {
                return (
                  <LimitsForm
                    {...props}
                    dispatch={parentDispatch}
                    disabled={apiLimitsDisabled}
                    setLoading={setLoading}
                  />
                );
              }}
            </TableForm>
            <TableLegend>
              <ul className={styles.legend}>
                <li>
                  <Legends.Enabled />: enabled
                </li>
                <li>
                  <Legends.Disabled />: disabled
                </li>
                <li>
                  <Legends.Global />: global setting
                </li>
              </ul>
            </TableLegend>
          </Table>
        </div>
      </>
    );

  return <div>Loading..</div>;
};

export default LimitsTable;
