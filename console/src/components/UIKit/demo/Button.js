import React from 'react';

import { Flex } from './styles';
import { Button } from './styles/Button.style';

// Buttons *************************** //

export const Buttons = () => (
  <React.Fragment>
    <Flex
      display="flex"
      justifyContent="flex-start"
      mb="lg" // ~ margin-bottom: theme.space.lg
    >
      {/* Primary button */}
      <Button
        bg="yellow.primary" // ~ background-color: theme.colors.yellow.primary
        color="black.text" // ~ theme.colors.black.text
        height={1} // button height ~ theme.sizes[1]
        px="lg" // padding(X-axis) ~ theme.space.lg
        border={1} // border ~ theme.borders[1]
        borderColor="yellow.primary" // ~ theme.colors.yellow.primary
        fontWeight="bold" // ~ theme.fontWeights.bold
        fontSize="button" // ~ theme.fontSizes.button
        borderRadius="xs" // ~ theme.raddi.xs
        mr="lg" // ~ margin-right: theme.space.lg
      >
        Primary button
      </Button>
      {/* Secondary button */}
      <Button
        bg="white"
        color="black.text"
        height={1}
        px="lg"
        border={1}
        borderColor="black.secondary"
        fontWeight="bold"
        fontSize="button"
        borderRadius="xs"
      >
        Secondary button
      </Button>
    </Flex>
    {/* Second row ~ small primary buttons */}
    <Flex display="flex" justifyContent="flex-start">
      <Button
        bg="yellow.primary"
        color="white"
        height={0}
        px="md"
        border={1}
        fontSize="button"
        fontWeight="medium"
        borderColor="yellow.primary"
        borderRadius="xs"
        mr="lg"
      >
        Primary button
      </Button>
      <Button
        bg="green.primary"
        color="white"
        height={0}
        px="md"
        border={1}
        fontSize="button"
        fontWeight="medium"
        borderColor="green.primary"
        borderRadius="xs"
        mr="lg"
      >
        Primary button
      </Button>
      <Button
        bg="red.primary"
        color="white"
        height={0}
        px="md"
        border={1}
        fontSize="button"
        fontWeight="medium"
        borderColor="red.primary"
        borderRadius="xs"
        mr="lg"
      >
        Primary button
      </Button>
      <Button
        bg="orange.primary"
        color="white"
        height={0}
        px="md"
        border={1}
        fontSize="button"
        fontWeight="medium"
        borderColor="orange.primary"
        borderRadius="xs"
        mr="lg"
      >
        Primary button
      </Button>
      <Button
        bg="blue.primary"
        color="white"
        height={0}
        px="md"
        border={1}
        fontSize="button"
        fontWeight="medium"
        borderColor="blue.primary"
        borderRadius="xs"
      >
        Primary button
      </Button>
    </Flex>
  </React.Fragment>
);
