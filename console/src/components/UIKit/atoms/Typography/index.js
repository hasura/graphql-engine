import React from 'react';
import PropTypes from 'prop-types';

import { Heading, TextStyles, TextLinkStyles } from './Typography.style';

// Text Component *************** //

const Text = props => {
  const { children, type, fontWeight, fontSize } = props;

  /* Explainer Text
   *  lineHeight: 'explain'
   *  fontSize: 'explain'
   *  fontWeight: 'bold'
   */

  const lineHeight = type === 'explain' ? 'body' : 'explain';

  // fontWeight & fontSize value can be received as prop to override default styles.

  let fontWeightValue;
  let fontSizeValue;

  if (fontWeight) {
    fontWeightValue = fontWeight;
  } else if (type === 'explain') {
    fontWeightValue = 'bold';
  }
  // No else clause here.

  if (fontSize) {
    fontSizeValue = fontSize;
  } else {
    fontSizeValue = type === 'explain' ? 'explain' : 'p';
  }

  // ************************* //

  return (
    <TextStyles
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSizeValue}
      fontWeight={fontWeightValue}
    >
      {children}
    </TextStyles>
  );
};

// PropTypes for Text *********** //

Text.propTypes = {
  color: PropTypes.string,
  mb: PropTypes.string,
  mt: PropTypes.string,
  ml: PropTypes.string,
  mr: PropTypes.string,
};

// Default Props for Text ****** //

Text.defaultProps = {
  mb: 'zero',
  mt: 'zero',
  mr: 'zero',
  ml: 'zero',
  color: 'black.text',
};

// Anchor / Text Links ******** //

const TextLink = props => {
  const { children, underline } = props;

  return (
    <TextLinkStyles
      {...props}
      //   Based on underline prop.
      borderBottom={underline && 2}
      borderColor={underline && 'yellow.primary'}
    >
      {children}
    </TextLinkStyles>
  );
};

// PropTypes for TextLink ******** //

Text.propTypes = {
  color: PropTypes.string,
  fontWeight: PropTypes.string,
  fontSize: PropTypes.string,
};

// Default Props for TextLink ***** //

TextLink.defaultProps = {
  color: 'black.text',
  fontWeight: 'medium',
  fontSize: 'p',
};

// ********************************* //

export { Heading, Text, TextLink };
