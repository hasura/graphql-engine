import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';

import BaseStyles from './BaseStyles';

import {
  UIKitWrapperDiv,
  ColorSchemeDivWrapper,
  ColorSchemeDiv,
  ButtonsWrapper,
  BoxShadowDivWrapper,
  BoxShadowDiv,
  TextLinksWrapper,
  BrushWrapper,
  Brush,
} from './UIKit';

import { Button } from './Button';
import { AlertMessageBox } from './Alert';
import { Text, Heading } from './Text';

// Color Scheme ******************************** //

const ColorScheme = () => (
  <ColorSchemeDivWrapper>
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
  </ColorSchemeDivWrapper>
);

// Shades ******************************** //

const Shades = () => (
  <React.Fragment>
    <BrushWrapper my="lg">
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
    </BrushWrapper>
    <BrushWrapper my="lg">
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
    </BrushWrapper>
    <BrushWrapper my="lg">
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
    </BrushWrapper>
    <BrushWrapper my="lg">
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
    </BrushWrapper>
    <BrushWrapper my="lg">
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
    </BrushWrapper>
    <BrushWrapper my="lg">
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
    </BrushWrapper>
  </React.Fragment>
);

// Buttons *************************** //

const Buttons = () => (
  <React.Fragment>
    <ButtonsWrapper
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
    </ButtonsWrapper>
    {/* Second row ~ small primary buttons */}
    <ButtonsWrapper>
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
    </ButtonsWrapper>
  </React.Fragment>
);

// Box Shadows ***************************** //

const BoxShadows = () => (
  <BoxShadowDivWrapper>
    <BoxShadowDiv
      boxShadow={1} // box-shadow ~ theme.shadows[1]
      borderRadius="xs" // ~ theme.radii.xs
    />
    <BoxShadowDiv boxShadow={2} borderRadius="xs" />
    <BoxShadowDiv boxShadow={3} borderRadius="xs" />
    <BoxShadowDiv boxShadow={4} borderRadius="xs" />
  </BoxShadowDivWrapper>
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
    <TextLinksWrapper
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
    </TextLinksWrapper>
    {/* Underlined Text */}
    <TextLinksWrapper>
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
    </TextLinksWrapper>
  </React.Fragment>
);

// Headings ***************************** //

const Headings = () => (
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
    {/* Base styling */}
    <BaseStyles />
    <h1>UI Elements</h1>
    <h3>Colors</h3>
    <ColorScheme />
    <Shades />
    <h3>Buttons</h3>
    <Buttons />
    <h3>Shadows</h3>
    <BoxShadows />
    <h3>Alerts</h3>
    <Alerts />
    <h3>Text Links</h3>
    <TextLinks />
    <h3>Headings</h3>
    <Headings />
  </UIKitWrapperDiv>
);

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
