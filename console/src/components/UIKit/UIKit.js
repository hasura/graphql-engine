import styled from 'styled-components';
import { typography, color, border, shadow } from 'styled-system';

// Base Div ********************* //

export const BaseDiv = styled.div`
  display: flex;
  justify-content: flex-start;
`;

// ******************************** //

export const UIKitDiv = styled.div`
  padding: 3rem 5rem 10rem;

  h3 {
    margin: 2.5rem 0;
  }

  /* Roboto Font */

  @import url('https://fonts.googleapis.com/css?family=Roboto:300,400,500,700,900');

  ${typography}
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
  margin-bottom: 4rem;

  /* Demo ~ Inline buttons separation */

  button {
    margin-right: 3rem;
  }
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

export const TextLinksWrapper = styled(BaseDiv)``;
