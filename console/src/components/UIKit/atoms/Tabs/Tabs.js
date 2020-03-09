import styled, { css } from 'styled-components';
import {
  typography,
  border,
  flexbox,
  layout,
  space,
  color,
  shadow,
} from 'styled-system';

// ********************************* //

const StyledTab = styled.div`
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;

// ********************************* //

const StyledTabList = styled.ul`
  list-style-type: none;

  ${border}
  ${flexbox}
  ${layout}
  ${space}
  ${typography}
`;

StyledTabList.defaultProps = {
  borderBottom: 1,
  borderColor: 'grey.border',
  display: 'flex',
  justifyContent: 'flex-start',
  alignItems: 'center',
  px: 0,
};

// *********************************** //

const StyledTabListItem = styled.li`
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

StyledTabListItem.defaultProps = {
  fontSize: 'tab',
  mr: 40,
  pb: 'sm',
  fontWeight: 'medium',
  borderBottom: 4,
  borderColor: 'transparent',
  color: 'grey.tab',
};

// ***************************** //

const StyledTabContent = styled.div`
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;

// ********************************* //

export { StyledTab, StyledTabList, StyledTabListItem, StyledTabContent };
