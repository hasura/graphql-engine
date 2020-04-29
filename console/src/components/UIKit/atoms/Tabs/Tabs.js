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

const StyledTab = styled.div`
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;

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
const selectedBorderStyles = css`
  border-color: ${props => props.theme.colors.tab};
`;

const StyledTabListItem = styled.li`
  cursor: pointer;

  &:hover {
    color: ${props => props.theme.colors.black.text};
  }

  ${typography}
  ${space}
  ${color}
  ${border}

  ${props => (props.selected ? selectedBorderStyles : '')};
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

const StyledTabContent = styled.div`
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;

export { StyledTab, StyledTabList, StyledTabListItem, StyledTabContent };
