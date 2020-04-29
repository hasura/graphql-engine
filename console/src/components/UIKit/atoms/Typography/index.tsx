import React from 'react';

import { StyledHeading, StyledText } from './Typography';
import { Theme } from '../../theme';

export const Heading = props => {
  const { children, type } = props;

  if (type === 'subHeading') {
    return (
      <StyledHeading
        as="h4"
        fontSize="15px"
        pb="20px"
        mt="0px"
        mb="0px"
        {...props}
      >
        {children}
      </StyledHeading>
    );
  }

  return <StyledHeading {...props}>{children}</StyledHeading>;
};

Heading.defaultProps = {
  color: 'black.text',
  fontWeight: 'bold',
};

/**
 * @example
 *  Explainer Text
 *  lineHeight: 'explain'
 *  fontSize: 'explain'
 *  fontWeight: 'bold'
 */
export type TextProps = {
  type: keyof Theme['lineHeights'];
  fontWeight: keyof Theme['fontWeights'];
  fontSize: keyof Theme['fontSizes'];
  mb: keyof Theme['space'];
  mt: keyof Theme['space'];
  mr: keyof Theme['space'];
  ml: keyof Theme['space'];
};

export const Text: React.FC<TextProps> = props => {
  const { children, type, fontWeight, fontSize } = props;

  const lineHeight = type === 'explain' ? 'explain' : 'body';

  const fontWeightValue = type === 'explain' ? 'bold' : '';

  const fontSizeValue = type === 'explain' ? 'explain' : '';

  return (
    <StyledText
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSize || fontSizeValue}
      fontWeight={fontWeight || fontWeightValue}
    >
      {children}
    </StyledText>
  );
};

Text.defaultProps = {
  mb: 0,
  mt: 0,
};
