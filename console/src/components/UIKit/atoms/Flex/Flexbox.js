import styled from 'styled-components';
import { flexbox } from 'styled-system';

import { Box } from '../Box';

export const Flex = styled(Box)`
  ${flexbox}
`;

Flex.defaultProps = {
  alignItems: 'center',
  justifyContent: 'center',
};
