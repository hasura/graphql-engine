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
} from './UIKit';

import { Button } from './Button';
import { AlertMessageBox } from './Alert';
import { Text, Heading } from './Text';

// Color Scheme ******************************** //

const ColorScheme = () => (
  <ColorSchemeDivWrapper>
    <ColorSchemeDiv
      // ~ background: theme.colors.yellow.active
      bg="yellow.active"
      // ~ border-radius: theme.raddi[2]
      borderRadius={2}
    />
    <ColorSchemeDiv bg="black.active" borderRadius={2} />
    <ColorSchemeDiv bg="green.active" borderRadius={2} />
    <ColorSchemeDiv bg="red.active" borderRadius={2} />
    <ColorSchemeDiv bg="blue.active" borderRadius={2} />
    <ColorSchemeDiv bg="orange.active" borderRadius={2} />
  </ColorSchemeDivWrapper>
);

// Buttons *************************** //

const Buttons = () => (
  <React.Fragment>
    <ButtonsWrapper
      mb={4} // ~ margin-bottom: theme.space[4]
    >
      {/* Primary button */}
      <Button
        bg="yellow.active" // ~ background-color: theme.colors.yellow.active
        color="black.active" // ~ theme.colors.black.active
        height={1} // button height ~ theme.sizes[1]
        px={4} // padding(X-axis) ~ theme.space[4]
        border={1} // border ~ theme.borders[1]
        borderColor="yellow.active" // ~ theme.colors.yellow.active
        fontWeight={6} // ~ theme.fontWeights[6]
        fontSize={1} // ~ theme.fontSizes[1]
        borderRadius={1} // ~ theme.raddi[1]
        mr={4} // ~ margin-right: theme.space[4]
      >
        Primary button
      </Button>
      {/* Secondary button */}
      <Button
        bg="white"
        color="black.active"
        height={1}
        px={4}
        border={1}
        borderColor="black.active"
        fontWeight={6}
        fontSize={1}
        borderRadius={1}
      >
        Secondary button
      </Button>
    </ButtonsWrapper>
    {/* Second row ~ small primary buttons */}
    <ButtonsWrapper>
      <Button
        bg="yellow.active"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="yellow.active"
        borderRadius={1}
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="green.active"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={6}
        borderColor="green.active"
        borderRadius={1}
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="red.active"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="red.active"
        borderRadius={1}
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="orange.active"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="orange.active"
        borderRadius={1}
        mr={4}
      >
        Primary button
      </Button>
      <Button
        bg="blue.active"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="blue.active"
        borderRadius={1}
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
      borderRadius={1} // ~ theme.radii[1]
    />
    <BoxShadowDiv
      boxShadow={1} // box-shadow ~ theme.shadows[1]
      borderRadius={1} // ~ theme.radii[1]
    />
    <BoxShadowDiv
      boxShadow={2} // box-shadow ~ theme.shadows[2]
      borderRadius={1} // ~ theme.radii[1]
    />
    <BoxShadowDiv
      boxShadow={3} // box-shadow ~ theme.shadows[3]
      borderRadius={1} // ~ theme.radii[1]
    />
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
      borderColor="green.active" // ~ theme.colors.green.active
      borderRadius={1} // ~ theme.radii[1]
      boxShadow={1} // ~ theme.shadows[1]
      my={4} // margin-y-axis ~ theme.space[4])
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="blue.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="blue.active"
      borderRadius={1}
      boxShadow={1}
      my={4}
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="orange.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="orange.active"
      borderRadius={1}
      boxShadow={1}
      my={4}
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="red.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="red.active"
      borderRadius={1}
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
        fontWeight={4} // ~ theme.fontWeights[4]
        mr={4} // ~ theme.space[4]
        color="black.text" // ~theme.colors.black.text
      >
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} mr={4} color="green.active">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} mr={4} color="blue.active">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} mr={4} color="orange.active">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} color="red.active">
        Check it out
      </Text>
    </TextLinksWrapper>
    {/* Underlined Text */}
    <TextLinksWrapper>
      <Text
        fontSize={2} // ~ theme.fontSizes[2]
        fontWeight={4} // ~ theme.fontWeights[4]
        mr={4} // ~ theme.space[4]
        color="black.text" // ~theme.colors.black.text
        borderBottom={2} // ~ theme.borders[2]
        borderColor="yellow.active" // ~ theme.colors.yellow.active
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        mr={4}
        color="green.active"
        borderBottom={2}
        borderColor="green.active"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        mr={4}
        color="blue.active"
        borderBottom={2}
        borderColor="blue.active"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        mr={4}
        color="orange.active"
        borderBottom={2}
        borderColor="orange.active"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        color="red.active"
        borderBottom={2}
        borderColor="red.active"
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
    <Heading as="h2" mb={3} color="black.text" fontSize={5}>
      Subpage title
    </Heading>
    <Heading
      as="h3"
      mb={3}
      color="black.text"
      fontSize={4}
      mt={0} // ~ Overriding base styling.
    >
      Section Header
    </Heading>
    <Heading as="h4" color="black.text" fontSize={3}>
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
  >
    {/* Base styling */}
    <BaseStyles />
    <h1>UI Elements</h1>
    <h3>Colors</h3>
    <ColorScheme />
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
