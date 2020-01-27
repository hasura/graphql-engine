import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';

import {
  UIKitDiv,
  ColorSchemeDivWrapper,
  ColorSchemeDiv,
  ButtonsWrapper,
} from './UIKit';
import { Button } from './Button';

const UIKit = () => (
  <UIKitDiv>
    <h1>UI Elements</h1>
    <h3>Colors</h3>
    <ColorSchemeDivWrapper>
      <ColorSchemeDiv bg="yellows.1" borderRadius={2} />
      <ColorSchemeDiv bg="blacks.1" borderRadius={2} />
      <ColorSchemeDiv bg="greens.1" borderRadius={2} />
      <ColorSchemeDiv bg="reds.1" borderRadius={2} />
      <ColorSchemeDiv bg="blues.1" borderRadius={2} />
      <ColorSchemeDiv bg="oranges.1" borderRadius={2} />
    </ColorSchemeDivWrapper>
    <h3>Buttons</h3>
    {/* First row ~ large buttons */}
    <ButtonsWrapper>
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
        hoverColor="yellows.1"
      >
        Primary button
      </Button>
    </ButtonsWrapper>
    {/* Second row ~ small buttons */}
    <ButtonsWrapper>
      <Button
        bg="yellows.1"
        color="blacks.1"
        height={0}
        px={3}
        border={1}
        borderColor="yellows.1"
        borderRadius={1}
      >
        Primary button
      </Button>
    </ButtonsWrapper>
  </UIKitDiv>
);

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
