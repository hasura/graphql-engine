import styled from 'styled-components';
import { flexbox } from 'styled-system';

// Box ~ Styled-System
import { BaseStyledDiv } from '../Common.style';

export const AlertBoxStyles = styled(BaseStyledDiv)`
  /* Styled-System */
  ${flexbox};

  /* Alert type text */

  span {
    text-transform: capitalize;
  }
`;
