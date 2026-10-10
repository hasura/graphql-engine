import React, { useState } from 'react';
import { FaGlobe } from 'react-icons/fa';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import LimitsFormWrapper from './LimitsFormWrapper';
import { getLimitsforRole, RoleLimits, RoleState } from './utils';
import type { Limit } from './LimitsForm';
import {
  useMetadata,
  useUpdateAPILimits,
  type ApiLimitInput,
} from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { Flex, Text } from '@radix-ui/themes';
import { Button, Input, Switch, Table } from '@hasura/shared/ui';
import SecurityLegends, { Legends } from './SecurityLegends';

interface Props {
  headers: string[];
  keys: string[];
}

const formatUniqueParams = (uniqueParams?: 'IP' | string[] | null) =>
  uniqueParams
    ? ` (on ${uniqueParams === 'IP' ? 'IP Address' : uniqueParams.join(', ')})`
    : null;

const LimitCell: React.FC<{ state: RoleState; limit?: Limit | null }> = ({
  state,
  limit,
}) => {
  const isObjectLimit = typeof limit === 'object' && limit !== null;
  return (
    <Flex justify="center" align="center" gap="2">
      {state === RoleState.global ? (
        <Legends.Global />
      ) : state === RoleState.enabled ? (
        <Legends.Enabled />
      ) : (
        <Legends.Disabled />
      )}
      {state === RoleState.enabled && (
        <span>
          {isObjectLimit ? limit.max_reqs_per_min : limit}
          {isObjectLimit ? formatUniqueParams(limit.unique_params) : null}
        </span>
      )}
    </Flex>
  );
};

const LimitsTable: React.FC<Props> = ({ headers, keys }) => {
  const [loading, setLoading] = useState(false);
  const [newRole, setNewRole] = useState('');
  const [editingRole, setEditingRole] = useState<string | null>(null);
  const { data: meta, refetch: refetchMetadata } = useMetadata();
  const _updateAPILimits = useUpdateAPILimits();
  const getRowData = meta?.metadata ? getLimitsforRole(meta) : null;
  const apiLimits = meta?.metadata?.api_limits;
  const apiLimitsDisabled = apiLimits?.disabled ?? false;
  const roles = MetadataSelectors.getRoles(meta?.metadata);

  const updateAPILimits = (
    newAPILimits: ApiLimitInput['newAPILimits'],
    onSuccess?: () => void,
  ) => {
    return _updateAPILimits(
      {
        existingAPILimits: apiLimits,
        newAPILimits,
      },
      onSuccess,
    ).finally(() => {
      refetchMetadata();
    });
  };

  const updateGlobalAPISetting = (flag: boolean) => {
    updateAPILimits({
      disabled: flag,
    });
  };

  const closeForm = () => {
    setEditingRole(null);
    setNewRole('');
  };

  const addNewRole = () => {
    const role = newRole.trim();
    if (role) setEditingRole(role);
  };

  if (!getRowData || !meta) {
    return <div>Loading..</div>;
  }

  // `keys` maps the table columns (minus the role column) to the limit fields
  // returned by getLimitsforRole, in the same order.
  const getRoleLimits = (role: string) => {
    const rowData = getRowData(role);
    return keys.slice(1).reduce<Record<string, unknown>>((acc, key, i) => {
      acc[key] = rowData[i];
      return acc;
    }, {}) as RoleLimits;
  };

  const rowProps = (role: string) => ({
    className: 'cursor-pointer hover:bg-[var(--gray-a3)]',
    tabIndex: 0,
    'aria-selected': editingRole === role,
    onClick: () => setEditingRole(role),
    onKeyDown: (e: React.KeyboardEvent) => {
      if (e.key === 'Enter' || e.key === ' ') {
        e.preventDefault();
        setEditingRole(role);
      }
    },
  });

  return (
    <Analytics name="ApiLimits" {...REDACT_EVERYTHING}>
      <Flex direction="column" gap="4">
        <Switch
          disabled={loading}
          value={!apiLimitsDisabled}
          onChange={(value) => updateGlobalAPISetting(!value)}
        >
          Enable additional API limits.
        </Switch>
        <SecurityLegends />
        <Table.Root variant="surface">
          <Table.Header>
            <Table.Row>
              {headers.map((header, i) => (
                <Table.ColumnHeaderCell
                  key={header}
                  align={i === 0 ? 'left' : 'center'}
                >
                  {header}
                </Table.ColumnHeaderCell>
              ))}
            </Table.Row>
          </Table.Header>
          <Table.Body>
            <Table.Row>
              <Table.RowHeaderCell>admin</Table.RowHeaderCell>
              <Table.Cell align="center" colSpan={headers.length - 1}>
                <Text color="gray">full access</Text>
              </Table.Cell>
            </Table.Row>
            <Table.Row {...rowProps('global')}>
              <Table.RowHeaderCell>
                <Flex align="center" gap="2">
                  <FaGlobe />
                  Global
                </Flex>
              </Table.RowHeaderCell>
              {getRowData('global').map(({ global, state }, cellIndex) => (
                <Table.Cell key={`global-${keys[cellIndex + 1]}`}>
                  <LimitCell state={state} limit={global} />
                </Table.Cell>
              ))}
            </Table.Row>
            {roles.map((role) => (
              <Table.Row key={role} {...rowProps(role)}>
                <Table.RowHeaderCell>{role}</Table.RowHeaderCell>
                {getRowData(role).map(({ per_role, state }, cellIndex) => (
                  <Table.Cell key={`${role}-${keys[cellIndex + 1]}`}>
                    <LimitCell state={state} limit={per_role?.[role]} />
                  </Table.Cell>
                ))}
              </Table.Row>
            ))}
            <Table.Row>
              <Table.RowHeaderCell>
                <Flex gap="2" align="center">
                  <Input
                    value={newRole}
                    onChange={(e) => setNewRole(e.target.value)}
                    onKeyDown={(e) => {
                      if (e.key === 'Enter') addNewRole();
                    }}
                    placeholder="Enter new role"
                  />
                  <Button
                    size="sm"
                    mode="default"
                    disabled={!newRole.trim()}
                    onClick={addNewRole}
                  >
                    Configure
                  </Button>
                </Flex>
              </Table.RowHeaderCell>
              {headers.slice(1).map((header) => (
                <Table.Cell key={`new-role-${header}`}>
                  <Flex justify="center">
                    <Legends.Disabled />
                  </Flex>
                </Table.Cell>
              ))}
            </Table.Row>
          </Table.Body>
        </Table.Root>
      </Flex>
      {editingRole !== null && (
        <LimitsFormWrapper
          role={editingRole}
          currentData={getRoleLimits(editingRole)}
          apiLimits={apiLimits}
          disabled={apiLimitsDisabled}
          onClose={closeForm}
          setLoading={setLoading}
          updateAPILimits={updateAPILimits}
          refetchMetadata={refetchMetadata}
        />
      )}
    </Analytics>
  );
};

export default LimitsTable;
