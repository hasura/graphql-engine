import styled from 'styled-components';
import { typography, space, color, border, shadow } from 'styled-system';

// Parent Div ~ Global Styles ****************** //

export const UIKitWrapperDiv = styled.div`
  /* Roboto Font */

  @import url('https://fonts.googleapis.com/css?family=Roboto:300,400,500,700,900');

  ${typography}
  ${space}
`;

// Base Div ********************* //

export const BaseDiv = styled.div`
  display: flex;
  justify-content: flex-start;
`;

// ******************************** //

export const ColorSchemeDivWrapper = styled(BaseDiv)``;

// ******************************** //

export const ColorSchemeDiv = styled.div`
  width: 12rem;
  height: 12rem;
  margin-right: 3rem;

  ${color}
  ${border}
`;

// Extended wrapper div for buttons *********** //

export const ButtonsWrapper = styled(BaseDiv)`
  ${space}
`;

// ShadowDiv wrapper for box-shadows ********** //

export const BoxShadowDivWrapper = styled(BaseDiv)``;

// Shadow Div ********************************* //

export const BoxShadowDiv = styled.div`
  width: 22.5rem;
  height: 12.5rem;
  margin-right: 4rem;

  /* importing background color from theme */

  background: ${props => props.theme.colors.white};

  /* box-shadow ~ styled-system */
  ${shadow}
  /* border-radius ~ styled-system */
  ${border}
`;

// Text Links *********************************** //

export const TextLinksWrapper = styled(BaseDiv)`
  ${space}
`;
