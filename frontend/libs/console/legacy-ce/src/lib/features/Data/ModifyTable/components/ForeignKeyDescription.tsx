import {
  TableFkRelationships,
  generateForeignKeyLabel,
} from '@hasura/metadata/data-source';
import { Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export function ForeignKeyDescription({
  foreignKey,
}: {
  foreignKey: TableFkRelationships;
}) {
  const label = generateForeignKeyLabel(foreignKey);

  return (
    <Flex align="center" className="mb-2" gap="2">
      <Text weight="medium">{label}</Text>
      {foreignKey.name ? (
        <>
          -<Text className="italic">{foreignKey.name}</Text>
        </>
      ) : null}
    </Flex>
  );
}
