import styled from 'styled-components';
import {
  flexbox,
  typography,
  space,
  color,
  border,
  shadow,
  layout,
} from 'styled-system';

// Parent Div ~ Global Styles ************* //

export const UIKitWrapperDiv = styled.div`
  /* Roboto Font */

  @import url('https://fonts.googleapis.com/css?family=Roboto:300,400,500,700,900');

  ${typography}
  ${space}
  ${color}
`;

// Flexbox Div ********************** //

export const Flex = styled.div`
  ${flexbox}
  ${layout}
  ${space}
`;

// ******************************** //

export const ColorSchemeDiv = styled.div`
  width: 12rem;
  height: 12rem;
  margin-right: 3rem;

  ${color}
  ${border}
`;

// Shadow Div ********************************* //

export const BoxShadowDiv = styled.div`
  width: 22.5rem;
  height: 12.5rem;
  margin-right: 4rem;

  ${shadow}
  ${border}
  ${color}
`;

// Color Shades *************************** //

export const Brush = styled.div`
  ${color}
  ${border}
  ${space}
  ${layout}
`;
