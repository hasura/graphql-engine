import React from 'react';
import {
  CheckboxGroup,
  IconTooltip,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import { EVENT_TRIGGER_OPERATIONS } from '../../constants';
import { EventTriggerOperation } from '../../types';
import { capitalize } from 'inflection';
import { Flex, Strong } from '@radix-ui/themes';
import { Table } from '@hasura/shared/types';

import { getTableDisplayName } from '@hasura/shared/utils';
type OperationProps = {
  selectedOperations: EventTriggerOperation[];
  setOperations: (o: EventTriggerOperation[]) => void;
  readOnly: boolean;
  table: Table | null;
};

export const Operations: React.FC<OperationProps> = ({
  selectedOperations,
  setOperations,
  readOnly,
  table,
}) => {
  const allOperations = EVENT_TRIGGER_OPERATIONS.map((o) => ({
    value: o,
    disabled: readOnly,
    label:
      o === 'MANUAL' ? (
        <Flex align="center" gap="2">
          <Text>Via console</Text>
          <IconTooltip message="Trigger manually from table data browser in console" />
          <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/event-triggers/invoke-trigger-console.html" />
        </Flex>
      ) : (
        capitalize(o)
      ),
  }));

  return (
    <Flex align="center" className="mb-4" gap="4">
      <Text as="div">
        On <Strong>{table ? getTableDisplayName(table) : '--'}</Strong> table:
      </Text>
      <CheckboxGroup
        orientation="horizontal"
        gap="4"
        value={selectedOperations}
        onChange={(values) => setOperations(values as EventTriggerOperation[])}
        options={allOperations}
      />
    </Flex>
  );
};
