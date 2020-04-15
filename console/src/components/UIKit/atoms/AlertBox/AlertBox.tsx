import styled from 'styled-components';
import {
  flexbox,
  color,
  border,
  typography,
  layout,
  space,
  shadow,
} from 'styled-system';

interface Props {
  width: number;
  bg: string;
  borderRadius: string;
  fontSize: string;
  borderLeft: number;
  borderColor: string;
  boxShadow: number;
  height: string;
  pl: string;
  display: string;
  alignItems: string;
  color: string;
}

export const StyledAlertBox = styled.div<Props>`
  ${flexbox};
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}

  /* Alert type text */
  span {
    text-transform: capitalize;
  }
`;
