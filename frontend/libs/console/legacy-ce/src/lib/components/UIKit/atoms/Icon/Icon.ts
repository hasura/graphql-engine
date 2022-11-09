import styled from 'styled-components';
import { color, typography, layout, space, position } from 'styled-system';
import { IconProps } from './index';

export const StyledIcon = styled.svg<IconProps>`
  cursor: ${({ pointer }) => (pointer ? 'pointer' : '')};

  ${color}
  ${typography}
  ${layout}
  ${space}
  ${position}
`;
