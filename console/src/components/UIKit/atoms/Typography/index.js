import React from 'react';

import { Heading, TextStyles, TextLinkStyles } from './Typography.style';

// Text Component

const Text = props => {
  const { children, type } = props;

  /* Explainer Text
   *  lineHeight: 'explain'
   *  fontSize: 'explain'
   *  fontWeight: 'bold'
   */

  const lineHeight = type === 'explain' ? 'body' : 'explain';
  const fontSize = type === 'explain' ? 'explain' : 'p';
  const fontWeight = type === 'explain' && 'bold';

  // ************************* //

  return (
    <TextStyles
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSize}
      fontWeight={fontWeight}
    >
      {children}
    </TextStyles>
  );
};

// Default Props for Text

Text.defaultProps = {
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

// Default Props for TextLink ***** //

TextLink.defaultProps = {
  color: 'black.text',
  fontWeight: 'medium',
  fontSize: 'p',
};

// ********************************* //

export { Heading, Text, TextLink };
