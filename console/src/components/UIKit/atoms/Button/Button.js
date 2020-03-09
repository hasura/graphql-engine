import styled, { css } from 'styled-components';
import {
  layout,
  space,
  color,
  border,
  typography,
  flexbox,
} from 'styled-system';

export const StyledButton = styled.button`
  cursor: ${({ disabled }) => !disabled && 'pointer'};
  appearance: button;

  /* Pseudo classes like hover are not supported by Styled-System  */

  ${({ type, theme, disabled, boxShadowColor }) => {
    if (type === 'secondary' && !disabled) {
      return css`
        &:hover {
          box-shadow: ${`0 2px 8px 0 ${boxShadowColor}`};
          background: ${theme.colors.black.secondary};
          color: ${theme.colors.white};
        }
      `;
    } else if (!disabled) {
      return css`
        &:hover {
          box-shadow: ${`0 2px 8px 0 ${boxShadowColor}`};
        }
      `;
    }
    // No else clause here.
  }}

  &:disabled {
    cursor: not-allowed;
  }

  ${layout}
  ${space}
  ${typography}
  ${color}
  ${border}
  ${flexbox}
`;
