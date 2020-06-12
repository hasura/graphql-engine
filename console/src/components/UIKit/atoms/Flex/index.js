import styled from 'styled-components';

import { Box } from '../Box';

// TODO: use styled-system
export const Flex = styled(Box)`
  cursor: ${props => (props.pointer ? 'pointer' : '')};
  display: flex;
  align-items: center;
  justify-content: ${props =>
    props.justify ? props.justify : 'space-between'};
  border-bottom: ${props => (props.borderBottom ? props.borderBottom : '1px')};
  padding: 0.35em;
`;
