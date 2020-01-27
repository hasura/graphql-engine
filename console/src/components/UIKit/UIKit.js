import styled from 'styled-components';
import { color, border } from 'styled-system';

// ******************************** //

export const UIKitDiv = styled.div`
  padding: 3rem;

  h3 {
    margin: 2rem 0;
    font-weight: ${props => props.theme.fontWeights[5]};
  }
`;

// ******************************** //

export const ColorSchemeDivWrapper = styled.div`
  display: flex;
  justify-content: flex-start;
`;

// ******************************** //

export const ColorSchemeDiv = styled.div`
  width: 15rem;
  height: 15rem;
  margin-right: 2.5rem;

  ${color}
  ${border}
`;

// Extended wrapper div for buttons *********** //

export const ButtonsWrapper = styled(ColorSchemeDivWrapper)`
  margin-bottom: 2.5rem;

  /* Demo ~ Inline buttons separation */

  button {
    margin-right: 2.5rem;
  }
`;
