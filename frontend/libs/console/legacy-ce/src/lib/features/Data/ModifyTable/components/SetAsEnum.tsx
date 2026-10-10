import React from 'react';
import { Flex } from '@radix-ui/themes';
import { Switch, Text } from '@hasura/shared/ui';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { ModifyTableProps } from '../types';
import { Section } from '../parts';
import { useSetTableAsEnum } from '../hooks/useSetTableAsEnum';

/**
 * "Set Table as Enum" toggle. Rendered only when the resolved driver reports
 * `tables.modify.setAsEnum` support (see ModifyTable gating). Uses the
 * `<driver>_set_table_is_enum` metadata API.
 */
export const SetAsEnum: React.FC<ModifyTableProps> = ({ source, table }) => {
  const { data: isEnum = false } = useMetadata(
    (m) =>
      MetadataSelectors.findMetadataTable(source.name, table.table, m)
        ?.is_enum ?? false,
  );

  const { setTableAsEnum, isPending } = useSetTableAsEnum(
    source.name,
    table.table,
  );

  return (
    <Section
      headerText="Set Table as Enum"
      tooltipMessage="Expose this table as a GraphQL enum. The table must have a text-type primary key and (optionally) a comment column."
    >
      <Flex align="center" gap="3">
        <Switch
          value={isEnum}
          disabled={isPending}
          onChange={(checked) => setTableAsEnum(Boolean(checked))}
        />
        <Text>
          {isEnum
            ? 'This table is exposed as an enum.'
            : 'This table is not an enum.'}
        </Text>
      </Flex>
    </Section>
  );
};

export default SetAsEnum;
