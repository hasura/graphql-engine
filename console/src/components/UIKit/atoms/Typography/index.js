import React from 'react';
import PropTypes from 'prop-types';

import { Heading, TextStyles, TextLinkStyles } from './Typography.style';

// Text Component *************** //

const Text = props => {
  const { children, type, fontWeight } = props;

  /* Explainer Text
   *  lineHeight: 'explain'
   *  fontSize: 'explain'
   *  fontWeight: 'bold'
   */

  const lineHeight = type === 'explain' ? 'body' : 'explain';

  const fontSize = type === 'explain' ? 'explain' : 'p';

  // fontWeight value can be received as prop.

  let fontWeightValue;

  if (fontWeight) {
    fontWeightValue = fontWeight;
  } else if (type === 'explain') {
    fontWeightValue = 'bold';
  }
  // No else clause here.

  // ************************* //

  return (
    <TextStyles
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSize}
      fontWeight={fontWeightValue}
    >
      {children}
    </TextStyles>
  );
};

// PropTypes for Text *********** //

Text.propTypes = {
  color: PropTypes.string,
  m: PropTypes.string,
};

// Default Props for Text ****** //

Text.defaultProps = {
  m: 'zero',
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
