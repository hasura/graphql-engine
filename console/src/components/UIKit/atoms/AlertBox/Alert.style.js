import styled from 'styled-components';
import { flexbox } from 'styled-system';

// Box ~ Styled-System
import { BaseStyledDiv } from '../Common.style';

export const AlertBoxStyles = styled(BaseStyledDiv)`
  /* Styled-System */
  ${flexbox};

  /* Alert Icon */

  svg {
    font-size: 1.8rem;

    /* Icon color ~ based on Alert Type */

    color: ${({ theme, type }) =>
    (theme.alertBoxes[type] ? theme.alertBoxes[type].borderColor : 'black')};
  }

  span {
    text-transform: capitalize;
  }
`;
