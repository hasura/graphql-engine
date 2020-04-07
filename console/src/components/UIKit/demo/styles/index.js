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
  ${typography}
  ${space}
  ${color}
`;

// Heading ************************* //

export const Heading = styled.h1`
    ${typography}
    ${color}
    ${space}
`;

// Paragraph ************************* //

export const Text = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}  
`;

// Base Div *************************** //

export const Box = styled.div`
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;

// Flexbox Div ********************** //

export const Flex = styled(Box)`
  ${flexbox}
`;

Flex.defaultProps = {
  display: 'flex',
  alignItems: 'center',
};

/*
 * Extending Base Div ~ Box for readability
 * ColorSchemeDiv
 * BoxShadowDiv
 * Brush
 * AlertMessageBox
 */

// Color Scheme Div ******************** //

export const ColorSchemeDiv = styled(Box)``;

// Shadow Div ********************************* //

export const BoxShadowDiv = styled(Box)``;

// Color Shades *************************** //

export const Brush = styled(Box)``;

// Alert Box ****************************** //

export const AlertMessageBox = styled(Box)``;
