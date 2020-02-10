import styled, { css } from 'styled-components';
import {
  typography,
  border,
  flexbox,
  layout,
  space,
  color,
} from 'styled-system';

import { BaseStyledDiv } from '../Common.style';

// ********************************* //

const TabStyles = styled(BaseStyledDiv)``;

// ********************************* //

const TabList = styled.ul`
  list-style-type: none;

  /* Styled-System */
  ${border}
  ${flexbox}
  ${layout}
  ${space}
  ${typography}
`;

// Default props for TabList ******** //

TabList.defaultProps = {
  borderBottom: 1,
  borderColor: 'grey.border',
  display: 'flex',
  justifyContent: 'flex-start',
  alignItems: 'center',
  px: 0,
};

// *********************************** //

const TabListItem = styled.li`
  cursor: pointer;

  /* Hover */
  &:hover {
    color: ${props => props.theme.colors.black.text};
  }

  /* Styled-System */
  ${typography}
  ${space}
  ${color}
  ${border}

   /* Selected / active tab list item */

  ${props =>
    props.selected &&
    css`
      border-color: ${props.theme.colors.tab};
    `};

`;

// Default props for TabListItem ***** //

TabListItem.defaultProps = {
  fontSize: 'tab',
  mr: 40,
  pb: 'sm',
  fontWeight: 'medium',
  borderBottom: 4,
  borderColor: 'transparent',
  color: 'grey.tab',
};

// ***************************** //

const TabContent = styled(BaseStyledDiv)``;

// ********************************* //

export { TabStyles, TabList, TabListItem, TabContent };
