import React from 'react';
import { FaPlug, FaFont } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

const RsLeafCell = ({ leafName }: { leafName: React.ReactNode }) => (
  <Flex className="mr-2" align="center" gap="1">
    <FaFont title="Field" />
    <span>{leafName}</span>
  </Flex>
);

// the design mockup was using FA v5, instead of fa-project-diagram, I've used  fa-code-fork from FA v4 for the time being
// this matches with the icon that we show on RS page
// this can be changed once after we upgrade Font Awesome to v5
// edit: updated this to use FaPlug to have just one representation for remote schema in the table
const ToRsCell = ({
  rsName,
  leafs,
}: {
  rsName: React.ReactNode;
  leafs: React.ReactNode[];
}) => (
  <Flex align="center" gap="1">
    <FaPlug title="Remote schema" />
    {rsName}
    <span>/</span>
    {leafs.map((i, index) => (
      <RsLeafCell key={index} leafName={i} />
    ))}
  </Flex>
);

export default ToRsCell;
