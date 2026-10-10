import React, { ReactElement, useEffect } from 'react';
import { Button, Dialog, Link } from '@hasura/shared/ui';
import { Flex, Separator } from '@radix-ui/themes';
import { RoleLimits, RoleState, validateUniqueParams } from './utils';
import { isEmpty } from '@hasura/shared/utils';
import { LimitsForm } from './LimitsForm';
import { useRemoveAPILimits, type ApiLimitInput } from '@hasura/metadata/api';
import type { ApiLimits } from '@hasura/shared/types';
import useAPILimitForm from './hooks/useAPILimitForm';

export const labels: Record<
  keyof RoleLimits,
  { title: string; info: ReactElement<any> }
> = {
  depth_limit: {
    title: 'Depth Limit',
    info: <>Set the maximum relation depth a request can traverse.</>,
  },
  node_limit: {
    title: 'Node Limit',
    info: (
      <>Set the maximum number of nodes which can be requested in a request.</>
    ),
  },
  batch_limit: {
    title: 'Batch Request Limit',
    info: (
      <>
        Set the maximum number of operations that can be sent in a{' '}
        <Link
          href="https://hasura.io/docs/latest/api-reference/graphql-api/index/#batching-requests"
          target="_blank"
          rel="noopener noreferrer"
        >
          batch request.
        </Link>
      </>
    ),
  },
  rate_limit: {
    title: 'Request Rate Limit (Requests Per Minute)',
    info: (
      <>
        Set a{' '}
        <Link
          href="https://hasura.io/docs/latest/security/api-limits/#rate-limits"
          target="_blank"
          rel="noopener noreferrer"
        >
          request rate limit
        </Link>{' '}
        for this role. You can also combine additional unique parameters for
        more granularity.
      </>
    ),
  },
  time_limit: {
    title: 'Operation time limit (Seconds)',
    info: <>Global timeout for GraphQL operations.</>,
  },
};

interface LimitsFormWrapperProps {
  role: string;
  currentData: RoleLimits;
  disabled: boolean;
  apiLimits?: ApiLimits | undefined;
  onClose: () => void;
  setLoading: React.Dispatch<React.SetStateAction<boolean>>;
  updateAPILimits: (
    input: ApiLimitInput['newAPILimits'],
    onSuccess?: () => void,
  ) => Promise<void>;
  refetchMetadata: () => Promise<any>;
}

const LimitsFormWrapper: React.FC<LimitsFormWrapperProps> = ({
  role,
  currentData,
  disabled,
  apiLimits,
  onClose,
  setLoading,
  updateAPILimits,
  refetchMetadata,
}) => {
  const removeAPILimits = useRemoveAPILimits();
  const {
    rateLimit,
    nodeLimit,
    batchLimit,
    timeLimit,
    depthLimit,
    resetForm,
    getState,
    changeRoleState,
    updateGlobalUniqueParams,
    updateUniqueParams,
    changeRoleLimit,
    changeGlobalLimit,
    getApiLimitByKey,
  } = useAPILimitForm();

  // Only reset when the edited role changes: `currentData` is rebuilt on every
  // render of the table, and resetting on it would discard in-progress edits.
  useEffect(() => {
    resetForm(currentData);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [role]);

  const submit = (e?: React.SyntheticEvent) => {
    e?.preventDefault();
    if (saveDisabled) return;
    setLoading(true);
    updateAPILimits(getState(disabled), onClose).finally(() => {
      setLoading(false);
    });
  };

  const isDisabled = (currentRole: string) => {
    if (!currentRole) return true;
    const disabled_for_role =
      isEmpty(depthLimit?.global) &&
      isEmpty(nodeLimit?.global) &&
      isEmpty(batchLimit?.global) &&
      isEmpty(timeLimit?.global) &&
      isEmpty(rateLimit?.global);
    return currentRole !== 'global' && disabled_for_role;
  };

  const onUniqueParamsChange = (role: string) => (value: 'IP' | string[]) => {
    if (role === 'global') {
      return updateGlobalUniqueParams(value);
    }

    return updateUniqueParams({ limit: value, role });
  };

  const onRadioChange = (limit: keyof RoleLimits, state: RoleState) => () => {
    return changeRoleState(limit, state);
  };

  const onInputChange =
    (limit: keyof RoleLimits, role: string) =>
    (val: string): any => {
      const value = parseInt(val, 10);
      if (Number.isNaN(value)) return;
      if (role !== 'global') {
        return changeRoleLimit(limit, { role, limit: value });
      }

      return changeGlobalLimit(limit, value);
    };

  const removeBtnClickHandler = (role: string) => {
    removeAPILimits(
      {
        existingAPILimits: apiLimits,
        role,
      },
      () => {
        refetchMetadata();
        onClose();
      },
    );
  };

  const actionsDisabled = isDisabled(role);
  const hasInvalidUniqueParams =
    rateLimit.state === RoleState.enabled &&
    validateUniqueParams(
      role === 'global'
        ? rateLimit.global?.unique_params
        : rateLimit.per_role?.[role]?.unique_params,
    ) !== null;
  const saveDisabled = actionsDisabled || hasInvalidUniqueParams;

  return (
    <Dialog
      size="lg"
      title={role === 'global' ? 'Global Settings' : `Role: ${role}`}
      onClose={onClose}
      onOpenChange={(open) => {
        if (!open) onClose();
      }}
      footer={{
        callToAction: 'Save Settings',
        callToDeny: 'Cancel',
        onClose,
        onSubmit: submit,
        callToActionProps: { disabled: saveDisabled },
        leftContent: (
          <Button
            mode="destructive"
            disabled={actionsDisabled}
            onClick={() => removeBtnClickHandler(role)}
          >
            Remove Settings
          </Button>
        ),
      }}
    >
      <form onSubmit={submit}>
        <Flex direction="column" gap="4">
          {Object.entries(labels).map(([key, label], i) => {
            const limit = key as keyof RoleLimits;
            const apiLimit = getApiLimitByKey(limit);
            return (
              <React.Fragment key={key}>
                {i > 0 && <Separator size="4" />}
                <LimitsForm
                  limit={limit}
                  label={label}
                  role={role}
                  state={(apiLimit?.state as RoleState) ?? RoleState.global}
                  globalLimit={apiLimit?.global}
                  roleLimit={apiLimit?.per_role?.[role]}
                  unique_params_global={rateLimit?.global?.unique_params}
                  unique_params_role={
                    rateLimit?.per_role?.[role]?.unique_params
                  }
                  max_reqs_global={rateLimit?.global.max_reqs_per_min}
                  max_reqs_role={rateLimit?.per_role?.[role]?.max_reqs_per_min}
                  onInputChange={onInputChange}
                  onRadioChange={onRadioChange}
                  onUniqueParamsChange={onUniqueParamsChange}
                />
              </React.Fragment>
            );
          })}
        </Flex>
      </form>
    </Dialog>
  );
};

export default LimitsFormWrapper;
