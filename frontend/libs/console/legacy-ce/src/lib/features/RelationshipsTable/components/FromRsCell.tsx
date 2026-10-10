import React from 'react';
import { FaFont } from 'react-icons/fa';
import { FiType } from 'react-icons/fi';
import { Flex } from '@radix-ui/themes';

const RsLeafCell = ({ leafName }: { leafName: React.ReactNode }) => (
  <Flex align="center" gap="1">
    <FaFont title="Field" />
    <span>{leafName}</span>
  </Flex>
);

// the desgin mockup was using FA v5, instead of fa-project-diagram, I've used  fa-code-fork from FA v4 for the time being
// this matches with the icon that we show on RS page
// this can be changed once after we upgrade Font Awesome to v5
const FromRsCell = ({
  leafs,
  rsType,
}: {
  rsType: React.ReactNode;
  leafs: React.ReactNode[];
}) => (
  <Flex align="center" gap="1">
    <FiType title="Type" />
    {rsType}
    <span className="px-2">/</span>
    {leafs.map((i, index) => (
      <RsLeafCell key={index} leafName={i} />
    ))}
  </Flex>
);

export default FromRsCell;
