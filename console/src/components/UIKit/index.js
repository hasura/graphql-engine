import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';

import {
  Flex,
  UIKitWrapperDiv,
  ColorSchemeDiv,
  BoxShadowDiv,
  Brush,
} from './UIKit';

import { Button } from './Button';
import { AlertMessageBox } from './Alert';
import { Text, Heading } from './Text';

// Color Scheme ******************************** //

const ColorScheme = () => (
  <Flex display="flex" justifyContent="flex-start">
    <ColorSchemeDiv
      // ~ background: theme.colors.yellow.primary
      bg="yellow.primary"
      // ~ border-radius: theme.raddi.md
      borderRadius="sm"
    />
    <ColorSchemeDiv bg="black.secondary" borderRadius="sm" />
    <ColorSchemeDiv bg="green.primary" borderRadius="sm" />
    <ColorSchemeDiv bg="red.primary" borderRadius="sm" />
    <ColorSchemeDiv bg="blue.primary" borderRadius="sm" />
    <ColorSchemeDiv bg="orange.primary" borderRadius="sm" />
  </Flex>
);

// Shades ******************************** //

const Shades = () => (
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

// Buttons *************************** //

const Buttons = () => (
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

// Box Shadows ***************************** //

const BoxShadows = () => (
  <Flex display="flex" justifyContent="flex-start">
    <BoxShadowDiv
      boxShadow={1} // box-shadow ~ theme.shadows[1]
      borderRadius="xs" // ~ theme.radii.xs
      bg="white" // ~ background: theme.colors.white
    />
    <BoxShadowDiv boxShadow={2} borderRadius="xs" bg="white" />
    <BoxShadowDiv boxShadow={3} borderRadius="xs" bg="white" />
    <BoxShadowDiv boxShadow={4} borderRadius="xs" bg="white" />
  </Flex>
);

// Alerts ***************************** //

const Alerts = () => (
  <React.Fragment>
    <AlertMessageBox
      minWidth={1 / 2} // width ~ 50%
      height={1} // ~ theme.sizes[1]
      bg="green.light" // ~ theme.colors.green.light
      maxWidth={866} // ~ max-width: 866px
      borderLeft={4} // ~ theme.borders[4]
      borderColor="green.primary" // ~ theme.colors.green.primary
      borderRadius="xs" // ~ theme.radii.xs
      boxShadow={2} // ~ theme.shadows[2]
      my="lg" // margin-y-axis ~ theme.space.lg
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="blue.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="blue.primary"
      borderRadius="xs"
      boxShadow={2}
      my="lg"
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="orange.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="orange.primary"
      borderRadius="xs"
      boxShadow={2}
      my="lg"
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="red.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="red.primary"
      borderRadius="xs"
      boxShadow={2}
      my="lg"
    />
  </React.Fragment>
);

// Text Links ***************************** //

const TextLinks = () => (
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

const Typography = () => (
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

// UIKit(Parent) Demo component *********** //

const UIKit = () => (
  <UIKitWrapperDiv
    fontFamily="roboto" // ~ theme.fonts.roboto
    p="lg" // ~ padding: theme.space.lg
    mb="xl" // ~ margin-bottom: theme.space.xl
    bg="white" // ~ theme.colors.white
  >
    <Heading color="black.text">UI Elements</Heading>
    <Heading as="h3" color="black.text" my="lg">
      Colors
    </Heading>
    <ColorScheme />
    <Shades />
    <Heading as="h3" color="black.text" my="lg">
      Buttons
    </Heading>
    <Buttons />
    <Heading as="h3" color="black.text" my="lg">
      Shadows
    </Heading>
    <BoxShadows />
    <Heading as="h3" color="black.text" my="lg">
      Alerts
    </Heading>
    <Alerts />
    <Heading as="h3" color="black.text" my="lg">
      Text Links
    </Heading>
    <TextLinks />
    <Heading as="h3" color="black.text" my="lg">
      Typography
    </Heading>
    <Typography />
  </UIKitWrapperDiv>
);

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
