import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';

import {
  UIKitDiv,
  ColorSchemeDivWrapper,
  ColorSchemeDiv,
  ButtonsWrapper,
  BoxShadowDivWrapper,
  BoxShadowDiv,
  TextLinksWrapper,
} from './UIKit';

import { Button } from './Button';
import { AlertMessageBox } from './Alert';
import { Text } from './Text';

// Color Scheme ******************************** //

const ColorScheme = () => (
  <ColorSchemeDivWrapper>
    <ColorSchemeDiv bg="yellows.1" borderRadius={2} />
    <ColorSchemeDiv bg="blacks.1" borderRadius={2} />
    <ColorSchemeDiv bg="greens.1" borderRadius={2} />
    <ColorSchemeDiv bg="reds.1" borderRadius={2} />
    <ColorSchemeDiv bg="blues.1" borderRadius={2} />
    <ColorSchemeDiv bg="oranges.1" borderRadius={2} />
  </ColorSchemeDivWrapper>
);

// Buttons *************************** //

const Buttons = () => (
  <React.Fragment>
    <ButtonsWrapper>
      {/* Primary button */}
      <Button
        bg="yellows.1" // background-color ~ theme.colors.yellows[1]
        color="blacks.1" // ~ theme.colors.blacks[1]
        height={1} // button height ~ theme.sizes[1]
        px={4} // padding(X-axis) ~ theme.space[4]
        border={1} // border ~ theme.borders[1]
        borderColor="yellows.1" // ~ theme.colors.yellows[1]
        fontWeight={6} // ~ theme.fontWeights[6]
        fontSize={1} // ~ theme.fontSizes[1]
        borderRadius={1} // ~ theme.raddi[1]
      >
        Primary button
      </Button>
      {/* Secondary button */}
      <Button
        bg="white"
        color="blacks.1"
        height={1}
        px={4}
        border={1}
        // borderColor='yellows.1'
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
        bg="yellows.1"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="yellows.1"
        borderRadius={1}
      >
        Primary button
      </Button>
      <Button
        bg="greens.1"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={6}
        borderColor="greens.1"
        borderRadius={1}
      >
        Primary button
      </Button>
      <Button
        bg="reds.1"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="reds.1"
        borderRadius={1}
      >
        Primary button
      </Button>
      <Button
        bg="oranges.1"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="oranges.1"
        borderRadius={1}
      >
        Primary button
      </Button>
      <Button
        bg="blues.1"
        color="white"
        height={0}
        px={3}
        border={1}
        fontWeight={5}
        borderColor="blues.1"
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
      bg="greens.3" // ~ theme.colors.greens[3]
      maxWidth={866} // ~ max-width: 866px
      borderLeft={4} // ~ theme.borders[4]
      borderColor="greens.1" // ~ theme.colors.greens[1]
      borderRadius={1} // ~ theme.radii[1]
      boxShadow={1} // ~ theme.shadows[1]
      my={4} // margin-y-axis ~ theme.space[4])
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="blues.3"
      maxWidth={866}
      borderLeft={4}
      borderColor="blues.1"
      borderRadius={1}
      boxShadow={1}
      my={4}
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="oranges.3"
      maxWidth={866}
      borderLeft={4}
      borderColor="oranges.1"
      borderRadius={1}
      boxShadow={1}
      my={4}
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="reds.3"
      maxWidth={866}
      borderLeft={4}
      borderColor="reds.1"
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
        color="blacks.2" // ~theme.colors.blacks[2]
      >
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} mr={4} color="greens.1">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} mr={4} color="blues.1">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} mr={4} color="oranges.1">
        Check it out
      </Text>
      <Text fontSize={2} fontWeight={4} color="reds.1">
        Check it out
      </Text>
    </TextLinksWrapper>
    {/* Underlined Text */}
    <TextLinksWrapper>
      <Text
        fontSize={2} // ~ theme.fontSizes[2]
        fontWeight={4} // ~ theme.fontWeights[4]
        mr={4} // ~ theme.space[4]
        color="blacks.2" // ~theme.colors.blacks[2]
        borderBottom={2} // ~ theme.borders[2]
        borderColor="yellows.1" // ~ theme.colors.yellow[1]
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        mr={4}
        color="greens.1"
        borderBottom={2}
        borderColor="greens.1"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        mr={4}
        color="blues.1"
        borderBottom={2}
        borderColor="blues.1"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        mr={4}
        color="oranges.1"
        borderBottom={2}
        borderColor="oranges.1"
      >
        Check it out
      </Text>
      <Text
        fontSize={2}
        fontWeight={4}
        color="reds.1"
        borderBottom={2}
        borderColor="reds.1"
      >
        Check it out
      </Text>
    </TextLinksWrapper>
  </React.Fragment>
);

// UIKit(Parent) Demo component *********** //

const UIKit = () => (
  <UIKitDiv
    fontFamily="roboto" // ~ theme.fonts.roboto
    p={4} // ~ padding: theme.space[4]
    mb={5} // ~ margin-bottom: theme.space[5]
  >
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
  </UIKitDiv>
);

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
