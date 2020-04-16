import styled from 'styled-components';
import { flexbox } from 'styled-system';

import { Box } from '../Box';

export const Flex = styled(Box)`
  cursor: ${props => (props.pointer ? 'pointer' : '')};
  ${flexbox}
`;

Flex.defaultProps = {
  alignItems: 'center',
  display: 'flex',
};
