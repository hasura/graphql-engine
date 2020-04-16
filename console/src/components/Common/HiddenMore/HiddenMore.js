import React from 'react';

import { Icon, Text, Flex } from '../../UIKit/atoms';

const HiddenMore = ({ title, more, expanded = false }) => {
  const [isExpanded, setIsExpanded] = React.useState(expanded);

  const toggle = () => setIsExpanded(!isExpanded);

  const getTitle = () => (
    <Flex mb="20px" pointer width="300px" onClick={toggle}>
      <Icon type={isExpanded ? 'down' : 'right'} mr="xs" />
      <Text fontWeight="bold" display="inline-block">
        {title}
      </Text>
    </Flex>
  );

  return (
    <div>
      {getTitle()}
      {isExpanded && more}
    </div>
  );
};

export default HiddenMore;
