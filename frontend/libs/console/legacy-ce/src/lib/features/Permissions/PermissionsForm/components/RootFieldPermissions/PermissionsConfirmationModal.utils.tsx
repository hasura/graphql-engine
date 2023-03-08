import React from 'react';
import { LS_KEYS, getLSItem } from '../../../../../utils/localStorage';

type Scenario = 'aggregate' | 'pk' | 'pks';

type GetPermissionsModalTitleArgs = {
  scenario: Scenario;
  role: string;
  primaryKeyColumns?: string;
};

export const getPermissionsModalTitle = ({
  scenario,
  role,
  primaryKeyColumns,
}: GetPermissionsModalTitleArgs) =>
  scenario === 'aggregate' ? (
    <>Are you sure you want to remove aggregation queries for role {role}?</>
  ) : (
    <>
      Are you sure you want to disable the access to column(s){' '}
      <span className="font-mono italic">{primaryKeyColumns}</span> for role{' '}
      {role}?
    </>
  );

export const getPermissionsModalDescription = (scenario: Scenario) =>
  scenario === 'aggregate' ? (
    <>
      <span className="font-mono italic">select_aggregate</span> will be
      disabled in GraphQL root field visibility since the permission is removed.
    </>
  ) : (
    <>
      <span className="font-mono italic">select_by_pk</span> will be disabled in
      GraphQL root field visibility since the primary key is disabled.
    </>
  );

export const getPermissionModalEnabled = () => {
  const status = getLSItem(LS_KEYS.permissionConfirmationModalStatus);
  const isEnabled = !status || status === 'enabled';
  return isEnabled;
};
