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
      // ~ border-radius: theme.raddi[2]
      borderRadius="sm" // ~ theme.raddi.md
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
    <BrushWrapper my={4}>
      <Brush
        bg="yellow.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="yellow.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="yellow.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
    </BrushWrapper>
    <BrushWrapper my={4}>
      <Brush
        bg="black.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="black.secondary"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="black.text"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
    </BrushWrapper>
    <BrushWrapper my={4}>
      <Brush
        bg="green.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="green.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="green.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="green.light"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
    </BrushWrapper>
    <BrushWrapper my={4}>
      <Brush
        bg="red.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="red.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush bg="red.hover" borderRadius="circle" width={1} height={1} mr={3} />
      <Brush bg="red.light" borderRadius="circle" width={1} height={1} mr={3} />
    </BrushWrapper>
    <BrushWrapper my={4}>
      <Brush
        bg="blue.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="blue.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="blue.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="blue.light"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
    </BrushWrapper>
    <BrushWrapper my={4}>
      <Brush
        bg="orange.original"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="orange.primary"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="orange.hover"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
      <Brush
        bg="orange.light"
        borderRadius="circle"
        width={1}
        height={1}
        mr={3}
      />
    </BrushWrapper>
  </React.Fragment>
);

// Buttons *************************** //

const Buttons = () => (
  <React.Fragment>
    <ButtonsWrapper
      mb={4} // ~ margin-bottom: theme.space[4]
    >
      {/* Primary button */}
      <Button
        bg="yellow.primary" // ~ background-color: theme.colors.yellow.primary
        color="black.text" // ~ theme.colors.black.text
        height={1} // button height ~ theme.sizes[1]
        px={4} // padding(X-axis) ~ theme.space[4]
        border={1} // border ~ theme.borders[1]
        borderColor="yellow.primary" // ~ theme.colors.yellow.primary
        fontWeight="bold" // ~ theme.fontWeights.bold
        fontSize={1} // ~ theme.fontSizes[1]
        borderRadius="xs" // ~ theme.raddi.xs
        mr={4} // ~ margin-right: theme.space[4]
      >
        Primary button
      </Button>
      {/* Secondary button */}
      <Button
        bg="white"
        color="black.text"
        height={1}
        px={4}
        border={1}
        borderColor="black.secondary"
        fontWeight="bold"
        fontSize={1}
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
        px={3}
        border={1}
        fontWeight="medium"
        borderColor="yellow.primary"
        borderRadius="xs"
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="green.primary"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight="medium"
        borderColor="green.primary"
        borderRadius="xs"
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="red.primary"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight="medium"
        borderColor="red.primary"
        borderRadius="xs"
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="orange.primary"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight="medium"
        borderColor="orange.primary"
        borderRadius="xs"
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="blue.primary"
        color="white"
        height={0}
        px={3}
        border={1}
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
      boxShadow={0} // box-shadow ~ theme.shadows[0]
      borderRadius="xs" // ~ theme.radii.xs
    />
    <BoxShadowDiv boxShadow={1} borderRadius="xs" />
    <BoxShadowDiv boxShadow={2} borderRadius="xs" />
    <BoxShadowDiv boxShadow={3} borderRadius="xs" />
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
      boxShadow={1} // ~ theme.shadows[1]
      my={4} // margin-y-axis ~ theme.space[4])
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="blue.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="blue.primary"
      borderRadius="xs"
      boxShadow={1}
      my={4}
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="orange.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="orange.primary"
      borderRadius="xs"
      boxShadow={1}
      my={4}
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="red.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="red.primary"
      borderRadius="xs"
      boxShadow={1}
      my={4}
    />
  </React.Fragment>
);

// Text Links ***************************** //

const TextLinks = () => (
  <React.Fragment>
    <TextLinksWrapper
      mb={2} // ~ margin-bottom: theme.space[3]
    >
      <Text
        fontSize={2} // ~ theme.fontSizes[2]
        fontWeight="medium" // ~ theme.fontWeights.medium
        mr={4} // ~ theme.space[4]
        color="black.text" // ~theme.colors.black.text
      >
        Check it out
      </Text>
      <Text fontSize={2} fontWeight="medium" mr={4} color="green.primary">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight="medium" mr={4} color="blue.primary">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight="medium" mr={4} color="orange.primary">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight="medium" color="red.primary">
        Check it out
      </Text>
    </TextLinksWrapper>
    {/* Underlined Text */}
    <TextLinksWrapper>
      <Text
        fontSize={2} // ~ theme.fontSizes[2]
        fontWeight="medium" // ~ theme.fontWeights.medium
        mr={4} // ~ theme.space[4]
        color="black.text" // ~theme.colors.black.text
        borderBottom={2} // ~ theme.borders[2]
        borderColor="yellow.primary" // ~ theme.colors.yellow.primary
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight="medium"
        mr={4}
        color="green.primary"
        borderBottom={2}
        borderColor="green.primary"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight="medium"
        mr={4}
        color="blue.primary"
        borderBottom={2}
        borderColor="blue.primary"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight="medium"
        mr={4}
        color="orange.primary"
        borderBottom={2}
        borderColor="orange.primary"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
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
      mb={3} // margin-bottom: theme.space[3]
      color="black.text" // ~ theme.colors.black.text
      fontSize={6} // ~ theme.fontSizes[6]
    >
      Main Heading
    </Heading>
    <Heading
      as="h2" // render as <h2> element
      mb={3}
      color="black.text"
      fontSize={5}
    >
      Subpage title
    </Heading>
    <Heading
      as="h3" // render as <h3> element
      mb={3}
      color="black.text"
      fontSize={4}
      mt={0} // ~ Overriding base styling.
    >
      Section Header
    </Heading>
    <Heading
      as="h4" // render as <h4> element
      color="black.text"
      fontSize={3}
    >
      Sub section Heading
    </Heading>
  </React.Fragment>
);

// UIKit(Parent) Demo component *********** //

const UIKit = () => (
  <UIKitWrapperDiv
    fontFamily="roboto" // ~ theme.fonts.roboto
    p={4} // ~ padding: theme.space[4]
    mb={5} // ~ margin-bottom: theme.space[5]
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
