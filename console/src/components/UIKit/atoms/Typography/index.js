import React from 'react';

import { StyledHeading, StyledText } from './Typography';

export const Heading = props => {
  const { children, type } = props;

  if (type === 'subHeading') {
    return (
      <StyledHeading
        as='h4'
        fontSize='15px'
        pb='20px'
        mt='0px'
        mb='0px'
        {...props}
      >
        {children}
      </StyledHeading>
    );
  }
  // No else block here.

  return <StyledHeading {...props}>{children}</StyledHeading>;
};

Heading.defaultProps = {
  color: 'black.text',
  fontWeight: 'bold'
};

/**
 * @example
 *  Explainer Text
 *  lineHeight: 'explain'
 *  fontSize: 'explain'
 *  fontWeight: 'bold'
 */
export const Text = props => {
  const { children, type, fontWeight, fontSize } = props;

  const lineHeight = type === 'explain' ? 'explain' : 'body';

  let fontWeightValue;
  let fontSizeValue;

  if (fontWeight) {
    fontWeightValue = fontWeight;
  } else if (type === 'explain') {
    fontWeightValue = 'bold';
  }

  if (fontSize) {
    fontSizeValue = fontSize;
  } else {
    fontSizeValue = type === 'explain' ? 'explain' : '';
  }

  return (
    <StyledText
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSizeValue}
      fontWeight={fontWeightValue}
    >
      {children}
    </StyledText>
  );
};

Text.defaultProps = {
  mb: 0,
  mt: 0
};
