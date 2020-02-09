import React from 'react';

import { Heading, Text, TextLinkStyles } from './Typography.style';

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
