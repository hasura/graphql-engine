import styled from 'styled-components';
import { color } from 'styled-system';

// ******************************** //

export const UIKitDiv = styled.div`
  padding: 3rem;

  h3 {
    margin: 2rem 0;
    font-weight: ${props => props.theme.fontWeights[5]};
  }
`;

// ******************************** //

export const ColorSchemeWrapper = styled.div`
  display: flex;
  justify-content: flex-start;
`;

// ******************************** //

export const ColorScheme = styled.div`
  width: 15rem;
  height: 15rem;
  margin-right: 2.5rem;

  ${color}
`;
