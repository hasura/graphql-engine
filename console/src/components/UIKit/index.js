import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';

import { UIKitDiv, ColorSchemeWrapper, ColorScheme } from './UIKit';

const UIKit = () => (
  <UIKitDiv>
    <h1>UI Elements</h1>
    <h3>Colors</h3>
    <ColorSchemeWrapper>
      <ColorScheme bg="yellows.1" />
      <ColorScheme bg="blacks.1" />
      <ColorScheme bg="greens.1" />
      <ColorScheme bg="reds.1" />
      <ColorScheme bg="blues.1" />
      <ColorScheme bg="oranges.1" />
    </ColorSchemeWrapper>
    <h3>Buttons</h3>
  </UIKitDiv>
);

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
