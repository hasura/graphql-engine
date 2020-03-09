import React from 'react';

import { Flex, BoxShadowDiv } from './styles';

// Box Shadows ***************************** //

export const BoxShadows = () => (
  <Flex display="flex" justifyContent="flex-start">
    <BoxShadowDiv
      width={225} // ~ 225px
      height={125} // ~ 125px
      boxShadow={1} // box-shadow ~ theme.shadows[1]
      borderRadius="xs" // ~ theme.radii.xs
      bg="white" // ~ background: theme.colors.white
      mr="lg" // ~ margin-right: theme.space.lg
    />
    <BoxShadowDiv
      boxShadow={2}
      borderRadius="xs"
      bg="white"
      mr="lg"
      width={225}
      height={125}
    />
    <BoxShadowDiv
      boxShadow={3}
      borderRadius="xs"
      bg="white"
      mr="lg"
      width={225}
      height={125}
    />
    <BoxShadowDiv
      boxShadow={4}
      borderRadius="xs"
      bg="white"
      width={225}
      height={125}
    />
  </Flex>
);
