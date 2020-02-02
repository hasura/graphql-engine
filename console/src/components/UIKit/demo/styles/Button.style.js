import styled from 'styled-components';
import {
  space,
  height,
  fontSize,
  fontWeight,
  color,
  border,
} from 'styled-system';

export const Button = styled.button`
// Base styles
cursor: pointer;

/* Theme configs */
  ${fontSize}
  ${fontWeight}
  ${color}
  ${space}
  ${height}
  ${border}
`;
