import { TbMathFunction } from 'react-icons/tb';
import { Flex } from '@radix-ui/themes';
import { TableFunction } from '@hasura/shared/types';
import { functionDisplayName } from '@hasura/metadata/helpers';
import { To } from 'react-router';
import { RelativeLink } from '@hasura/shared/ui';

export const FunctionDisplayName = ({
  dataSourceName,
  qualifiedFunction,
  to,
}: {
  dataSourceName?: string;
  qualifiedFunction: TableFunction;
  to?: To;
}) => {
  const content = (
    <Flex gap="1" align="center">
      <TbMathFunction className="text-muted mr-1" />
      {functionDisplayName({ dataSourceName, qualifiedFunction })}
    </Flex>
  );

  return to ? <RelativeLink to={to}>{content}</RelativeLink> : content;
};
