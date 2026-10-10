import { Flex, Heading } from '@radix-ui/themes';
import { SchemaDropdown } from './SchemaDropdown';
import { Source } from '@hasura/shared/types';

export function SourceName({ source }: { source: Source }) {
  return (
    <Flex align="center">
      <Flex align="center" gap="2" className="relative my-2">
        <SchemaDropdown source={source} />
        <Heading size="4">{source.name}</Heading>
      </Flex>
    </Flex>
  );
}
