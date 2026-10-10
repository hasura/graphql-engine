import { FaColumns, FaTable } from 'react-icons/fa';
import { Flex, TextProps } from '@radix-ui/themes';
import { RelationshipIcon, Text } from '@hasura/shared/ui';
import { getTableDisplayName } from '@hasura/shared/utils';
import { SuggestedRelationship } from '@hasura/metadata/api';

export const DisplaySuggestedRelationship = ({
  relationship,
  ...rest
}: {
  relationship: SuggestedRelationship;
} & TextProps) => (
  <Text asChild {...rest}>
    <Flex align="center" gap="2">
      <FaTable />
      <span>{getTableDisplayName(relationship.from.table)}</span>
      /
      <FaColumns />
      <span>{relationship.from.columns.join(' ')}</span>
      <RelationshipIcon
        type={relationship.type === 'array' ? 'one-to-many' : 'one-to-one'}
      />
      <FaTable />
      <span>{getTableDisplayName(relationship.to.table)}</span>
      /
      <FaColumns />
      {relationship.to.columns.join(' ')}
    </Flex>
  </Text>
);
