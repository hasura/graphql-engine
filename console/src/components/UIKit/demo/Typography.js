import React from 'react';

import { Flex, Text, Heading } from './styles';

// Text Links ***************************** //

export const TextLinks = () => (
  <React.Fragment>
    <Flex
      display="flex"
      justifyContent="flex-start"
      mb="xs" // ~ margin-bottom: theme.space.xs
    >
      <Text
        fontSize="p" // ~ theme.fontSizes.p
        fontWeight="medium" // ~ theme.fontWeights.medium
        mr="lg" // ~ theme.space.lg
        color="black.text" // ~theme.colors.black.text
      >
        Check it out
      </Text>
      <Text fontSize="p" fontWeight="medium" mr="lg" color="green.primary">
        Check it out
      </Text>
      <Text fontSize="p" fontWeight="medium" mr="lg" color="blue.primary">
        Check it out
      </Text>
      <Text fontSize="p" fontWeight="medium" mr="lg" color="orange.primary">
        Check it out
      </Text>
      <Text fontSize="p" fontWeight="medium" color="red.primary">
        Check it out
      </Text>
    </Flex>
    {/* Underlined Text */}
    <Flex
      display="flex"
      justifyContent="flex-start"
      mb="xs" // ~ margin-bottom: theme.space.xs
    >
      <Text
        fontSize="p" // ~ theme.fontSizes.p
        fontWeight="medium" // ~ theme.fontWeights.medium
        mr="lg" // ~ theme.space.lg
        color="black.text" // ~theme.colors.black.text
        borderBottom={2} // ~ theme.borders[2]
        borderColor="yellow.primary" // ~ theme.colors.yellow.primary
      >
        Check it out
      </Text>
      <Text
        fontSize="p"
        fontWeight="medium"
        mr="lg"
        color="green.primary"
        borderBottom={2}
        borderColor="green.primary"
      >
        Check it out
      </Text>
      <Text
        fontSize="p"
        fontWeight="medium"
        mr="lg"
        color="blue.primary"
        borderBottom={2}
        borderColor="blue.primary"
      >
        Check it out
      </Text>
      <Text
        fontSize="p"
        fontWeight="medium"
        mr="lg"
        color="orange.primary"
        borderBottom={2}
        borderColor="orange.primary"
      >
        Check it out
      </Text>
      <Text
        fontSize="p"
        fontWeight="medium"
        color="red.primary"
        borderBottom={2}
        borderColor="red.primary"
      >
        Check it out
      </Text>
    </Flex>
  </React.Fragment>
);

// Typography ***************************** //

export const Typography = () => (
  <React.Fragment>
    <Heading
      mb="md" // margin-bottom: theme.space.md
      color="black.text" // ~ theme.colors.black.text
      fontSize="h1" // ~ theme.fontSizes[6]
    >
      Main Heading
    </Heading>
    <Heading
      as="h2" // render as <h2> element
      mb="md"
      color="black.text"
      fontSize="h2"
    >
      Subpage title
    </Heading>
    <Heading
      as="h3" // render as <h3> element
      mb="md"
      color="black.text"
      fontSize="h3"
      mt={0} // ~ Overriding base styling.
    >
      Section Header
    </Heading>
    <Heading
      as="h4" // render as <h4> element
      color="black.text"
      fontSize="h4"
    >
      Sub section Heading
    </Heading>
    {/* Paragraph */}
    <Text
      my="md" // margin (y-axis): theme.space.md
      fontSize="p" // font-size: theme.space.fontSizes.p
      // line-height: theme.lineHeights.body
      lineHeight="body"
      color="black.text" // color: theme.colors.black.text
    >
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Semper quis lectus
      nulla at volutpat diam ut venenatis. Sed viverra tellus in hac habitasse
      platea dictumst. Id porta nibh venenatis cras. Velit dignissim sodales ut
      eu sem. Turpis cursus in hac habitasse platea dictumst quisque. Integer
      feugiat scelerisque varius morbi enim. Dui accumsan sit amet nulla. Donec
      et odio pellentesque diam volutpat commodo sed. Augue eget arcu dictum
      varius duis at. Nullam vehicula ipsum a arcu cursus vitae. Sapien et
      ligula ullamcorper malesuada proin libero nunc. Nunc congue nisi vitae
      suscipit tellus mauris a diam maecenas.
    </Text>
    {/* Explainer Text */}
    <Text
      my="md"
      // font-size: theme.fontSizes.explain
      fontSize="explain"
      fontWeight="bold"
      // line-height: theme.lineHeights.explain
      lineHeight="explain"
      color="black.text" // color: theme.colors.black.text
    >
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Semper quis lectus
      nulla at volutpat diam ut venenatis. Sed viverra tellus in hac habitasse
      platea dictumst. Id porta nibh venenatis cras. Velit dignissim sodales ut
      eu sem. Turpis cursus in hac habitasse platea dictumst quisque. Integer
      feugiat scelerisque varius morbi enim. Dui accumsan sit amet nulla. Donec
      et odio pellentesque diam volutpat commodo sed. Augue eget arcu dictum
      varius duis at. Nullam vehicula ipsum a arcu cursus vitae. Sapien et
      ligula ullamcorper malesuada proin libero nunc. Nunc congue nisi vitae
      suscipit tellus mauris a diam maecenas.
    </Text>
  </React.Fragment>
);
