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
} from './UIKit';
import { Button } from './Button';

// Color Scheme ************************************ //

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
    <BoxShadowDiv boxShadow={0} />
    <BoxShadowDiv boxShadow={1} />
    <BoxShadowDiv boxShadow={2} />
  </BoxShadowDivWrapper>
);

// UIKit(Parent) demo component *********** //

const UIKit = () => (
  <UIKitDiv>
    <h1>UI Elements</h1>
    <h3>Colors</h3>
    <ColorScheme />
    <h3>Buttons</h3>
    <Buttons />
    <h3>Shadows</h3>
    <BoxShadows />
  </UIKitDiv>
);

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
