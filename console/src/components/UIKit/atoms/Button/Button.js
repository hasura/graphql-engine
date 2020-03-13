import styled, { css } from 'styled-components';
import {
  layout,
  space,
  color,
  border,
  typography,
  flexbox,
} from 'styled-system';

const hoverStyles = ({ type, theme, disabled, boxShadowColor }) => {
  if (type === 'secondary' && !disabled) {
    return css`
      &:hover {
        box-shadow: 0 2px 8px 0 ${boxShadowColor};
        background: ${theme.colors.black.secondary};
        color: ${theme.colors.white};
      }
    `;
  } else if (!disabled) {
    return css`
      &:hover {
        box-shadow: 0 2px 8px 0 ${boxShadowColor};
      }
    `;
  }
  return '';
};

export const StyledButton = styled.button`
  appearance: button;
  
  cursor: ${({ disabled }) => !disabled && 'pointer'};
  &:disabled {
    cursor: not-allowed;
  }
  
  ${hoverStyles}
  ${layout}
  ${space}
  ${typography}
  ${color}
  ${border}
  ${flexbox}
`;
