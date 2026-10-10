import React, { ReactElement, useEffect, useState } from 'react';
import { Box, Flex, Text } from '@radix-ui/themes';
import { FaPlus, FaTimes } from 'react-icons/fa';
import {
  isReservedSessionVariable,
  RoleLimits,
  RoleState,
  validateUniqueParams,
} from './utils';
import { isEmpty } from '@hasura/shared/utils';
import {
  Button,
  Checkbox,
  IconButton,
  IconTooltip,
  Input,
  RadioGroup,
} from '@hasura/shared/ui';

export type Limit =
  | number
  | {
      unique_params?: 'IP' | string[] | null;
      max_reqs_per_min?: number;
    };

interface LimitFormProps {
  limit: keyof RoleLimits;
  label: {
    title: string;
    info: ReactElement<any>;
  };
  state: RoleState;
  roleLimit?: Limit;
  globalLimit?: Limit | null;
  unique_params_global?: 'IP' | string[] | null;
  unique_params_role?: 'IP' | string[] | null;
  max_reqs_global?: number;
  max_reqs_role?: number;
  role: string;
  onRadioChange: (limit: keyof RoleLimits, state: RoleState) => () => void;
  onUniqueParamsChange: (role: string) => (value: 'IP' | string[]) => void;
  onInputChange: (
    limit: keyof RoleLimits,
    role: string,
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
  const [addUniqueParams, setAddUniqueParams] = useState(false);
  const uniqueParams = isGlobal ? unique_params_global : unique_params_role;

  useEffect(() => {
    setAddUniqueParams(
      isGlobal ? !isEmpty(unique_params_global) : !isEmpty(unique_params_role),
    );
  }, [isGlobal, role, unique_params_global, unique_params_role]);

  const isdisabledGlobally =
    (isEmpty(globalLimit) || (globalLimit as number) < 0) && !isGlobal;

  const isDisabled =
    isdisabledGlobally ||
    state === RoleState.global ||
    state === RoleState.disabled;

  const renderValue = () => {
    const rateValue = isGlobal ? max_reqs_global : max_reqs_role;
    const othervalue = isGlobal
      ? (globalLimit as number) >= 0
        ? (globalLimit as number)
        : ''
      : (roleLimit as number);
    return isRateLimit ? (rateValue ?? '') : (othervalue ?? '');
  };

  const stateOptions = isGlobal
    ? [
        { value: RoleState.enabled, label: 'Enable' },
        { value: RoleState.disabled, label: 'Disable' },
      ]
    : [
        { value: RoleState.enabled, label: 'Custom' },
        { value: RoleState.global, label: 'Global' },
      ];

  const sessionVariablesDisabled = !addUniqueParams || isDisabled;
  const uniqueParamsError =
    isRateLimit && !isDisabled ? validateUniqueParams(uniqueParams) : null;

  // Keep at least one row so the "Session Variable(s)" option stays selected.
  const updateSessionVariables = (params: string[]) =>
    onUniqueParamsChange(role)(params.length > 0 ? params : ['']);

  const uniqueParamsType =
    uniqueParams === 'IP'
      ? 'IP'
      : !isEmpty(uniqueParams)
        ? 'session_variables'
        : '';

  return (
    <Flex
      gap="6"
      justify="between"
      align="start"
      wrap="wrap"
      className="overflow-hidden"
    >
      <Box className="flex-1 min-w-60">
        <Text as="div" size="2" weight="bold">
          {label.title}
        </Text>
        <Text as="div" size="2" color="gray">
          {label.info}
        </Text>
        {isdisabledGlobally && (
          <Text as="div" size="2" color="red" className="mt-2">
            Global Setting for{' '}
            <Text weight="bold">{label.title?.split('(')[0].trim()}</Text> needs
            to be set first
          </Text>
        )}
      </Box>
      <Flex direction="column" gap="3" className="w-72">
        <RadioGroup
          orientation="horizontal"
          disabled={isdisabledGlobally}
          value={state}
          options={stateOptions}
          onChange={(value) => onRadioChange(limit, value as RoleState)()}
        />
        <Input
          type="number"
          min={0}
          value={renderValue()}
          onChange={(e) => onInputChange(limit, role)(e.target.value)}
          placeholder={isRateLimit ? 'Request Per Minute' : 'Limit'}
          disabled={isDisabled}
        />
        {isRateLimit && (
          <>
            <Checkbox
              value={addUniqueParams}
              disabled={isDisabled}
              onChange={(checked) => setAddUniqueParams(checked === true)}
            >
              <Text weight="bold">Additional Unique Parameters</Text>
            </Checkbox>
            <RadioGroup
              orientation="horizontal"
              disabled={!addUniqueParams || isDisabled}
              value={uniqueParamsType}
              options={[
                { value: 'IP', label: 'IP Address' },
                {
                  value: 'session_variables',
                  label: (
                    <Flex align="center" gap="1">
                      Session Variable(s)
                      <IconTooltip message="Rate limit requests per unique combination of these session variable values" />
                    </Flex>
                  ),
                },
              ]}
              onChange={(value) =>
                onUniqueParamsChange(role)(value === 'IP' ? 'IP' : [''])
              }
            />
            {Array.isArray(uniqueParams) && uniqueParams.length > 0 && (
              <Flex direction="column" gap="2">
                {uniqueParams.map((param, index) => (
                  <Flex key={index} gap="2" align="center">
                    <Input
                      full
                      value={param}
                      disabled={sessionVariablesDisabled}
                      isInvalid={
                        !!uniqueParamsError &&
                        (!param.trim() || isReservedSessionVariable(param))
                      }
                      placeholder="x-hasura-user-id"
                      aria-label={`Session variable ${index + 1}`}
                      onChange={(e) =>
                        updateSessionVariables(
                          uniqueParams.map((p, i) =>
                            i === index ? e.target.value.trim() : p,
                          ),
                        )
                      }
                    />
                    <IconButton
                      variant="ghost"
                      color="gray"
                      icon={FaTimes}
                      radius="full"
                      aria-label={`Remove session variable ${index + 1}`}
                      disabled={sessionVariablesDisabled}
                      onClick={() =>
                        updateSessionVariables(
                          uniqueParams.filter((_, i) => i !== index),
                        )
                      }
                    />
                  </Flex>
                ))}
                <Box>
                  <Button
                    size="1"
                    mode="default"
                    leftIcon={FaPlus}
                    disabled={sessionVariablesDisabled}
                    onClick={() =>
                      updateSessionVariables([...uniqueParams, ''])
                    }
                  >
                    Add session variable
                  </Button>
                </Box>
                {uniqueParamsError && (
                  <Text as="div" size="2" color="red">
                    {uniqueParamsError}
                  </Text>
                )}
              </Flex>
            )}
          </>
        )}
      </Flex>
    </Flex>
  );
};
