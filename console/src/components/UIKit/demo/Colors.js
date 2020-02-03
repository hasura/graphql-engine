import React from 'react';

import { Flex, ColorSchemeDiv, Brush } from './styles';

// Color Scheme ******************************** //

export const ColorScheme = () => (
  <Flex display="flex" justifyContent="flex-start">
    <ColorSchemeDiv
      // ~ background: theme.colors.yellow.primary
      bg="yellow.primary"
      // ~ border-radius: theme.raddi.md
      borderRadius="sm"
      // ~ margin-right: theme.space.lg
      mr="lg"
      // width ~ '125px'
      width={125}
      // height ~ '125px
      height={125}
    />
    <ColorSchemeDiv
      bg="black.secondary"
      borderRadius="sm"
      mr="lg"
      width={125}
      height={125}
    />
    <ColorSchemeDiv
      bg="green.primary"
      borderRadius="sm"
      mr="lg"
      width={125}
      height={125}
    />
    <ColorSchemeDiv
      bg="red.primary"
      borderRadius="sm"
      mr="lg"
      width={125}
      height={125}
    />
    <ColorSchemeDiv
      bg="blue.primary"
      borderRadius="sm"
      mr="lg"
      width={125}
      height={125}
    />
    <ColorSchemeDiv
      bg="orange.primary"
      borderRadius="sm"
      width={125}
      height={125}
    />
  </Flex>
);

// Color Shades ******************************** //

export const Shades = () => (
  <React.Fragment>
    <Flex display="flex" justifyContent="flex-start" my="lg">
      <Brush
        bg="yellow.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="yellow.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="yellow.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
    </Flex>
    <Flex display="flex" justifyContent="flex-start" my="lg">
      <Brush
        bg="black.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="black.secondary"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="black.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="black.text"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
    </Flex>
    <Flex display="flex" justifyContent="flex-start" my="lg">
      <Brush
        bg="green.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="green.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="green.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="green.light"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
    </Flex>
    <Flex display="flex" justifyContent="flex-start" my="lg">
      <Brush
        bg="red.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="red.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="red.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="red.light"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
    </Flex>

    <Flex display="flex" justifyContent="flex-start" my="lg">
      <Brush
        bg="blue.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="blue.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="blue.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="blue.light"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
    </Flex>

    <Flex display="flex" justifyContent="flex-start" my="lg">
      <Brush
        bg="orange.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="orange.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="orange.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
      <Brush
        bg="orange.light"
        borderRadius="circle"
        width={1}
        height={1}
        mr="md"
      />
    </Flex>
  </React.Fragment>
);
