import React from 'react';

import { StyledHeading, StyledText, StyledTextLink } from './Typography';
import { Theme } from '../../theme';

export const Heading = StyledHeading;

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
  type?: keyof Theme['lineHeights'];
  fontWeight?: keyof Theme['fontWeights'];
  fontSize?: keyof Theme['fontSizes'];
  mb?: keyof Theme['space'];
  mt?: keyof Theme['space'];
  mr?: keyof Theme['space'];
  ml?: keyof Theme['space'];
  pl?: keyof Theme['space'];
  as?: keyof JSX.IntrinsicElements | React.ComponentType<any>;
  color?: keyof Theme['colors'];
  px?: keyof Theme['space'];
  py?: keyof Theme['space'];
};

export const Text: React.FC<TextProps> = props => {
  const { children, type, fontWeight, fontSize } = props;

  const lineHeight = type === 'explain' ? 'body' : 'explain';

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
    fontSizeValue = type === 'explain' ? 'explain' : 'p';
  }

  return (
    <StyledText
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSizeValue}
      fontWeight={fontWeightValue}
      color="black.text"
    >
      {children}
    </StyledText>
  );
};

Text.defaultProps = {
  mb: 'zero',
  mt: 'zero',
  mr: 'zero',
  ml: 'zero',
  pl: 'zero',
  fontWeight: 'normal',
  fontSize: 'p',
  color: 'black',
};

type TextLinkProps = {
  underline: boolean;
  color: string;
};

export const TextLink: React.FC<TextLinkProps> = props => {
  const { children, underline } = props;

  return (
    <StyledTextLink
      {...props}
      borderBottom={underline ? 2 : 'none'}
      borderColor={underline ? 'yellow.primary' : 'none'}
      fontWeight="medium"
      fontSize="p"
    >
      {children}
    </StyledTextLink>
  );
};

TextLink.defaultProps = {
  color: 'black.text',
};
