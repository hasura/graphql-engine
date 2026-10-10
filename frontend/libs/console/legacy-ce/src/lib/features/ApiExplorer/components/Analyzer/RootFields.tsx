import React from 'react';
import { FaTable } from 'react-icons/fa';
import type { ExplainResult } from './utils';
import { Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

type RootFieldsProps = {
  data: ExplainResult[];
  activeNode: number;
  onClick: (index: number) => void;
};

const RootFields: React.FC<RootFieldsProps> = ({
  data,
  activeNode,
  onClick,
}) => (
  <div className="ml-4">
    {data.map((analysis, i) => {
      return (
        analysis.field && (
          <Text
            as="div"
            className={i === activeNode ? 'cursor-pointer' : undefined}
            key={i}
            data-key={i}
            onClick={() => onClick(i)}
            weight={i === activeNode ? 'medium' : 'regular'}
            color={i === activeNode ? 'indigo' : 'gray'}
          >
            <Flex align="center" gap="2">
              <FaTable aria-hidden="true" />
              {analysis.field}
            </Flex>
          </Text>
        )
      );
    })}
  </div>
);

export default RootFields;
