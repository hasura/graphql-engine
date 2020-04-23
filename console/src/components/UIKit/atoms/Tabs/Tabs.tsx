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

import { IStyledTabListItemProp, IStyledTabListProp, styledTabListDefaultProps, styledTabListItemDefaultProps } from './typings'


const StyledTab = styled.div`
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;


const StyledTabList = styled.ul<IStyledTabListProp>`
  list-style-type: none;

  ${border}
  ${flexbox}
  ${layout}
  ${space}
  ${typography}
`;

StyledTabList.defaultProps = styledTabListDefaultProps

const selectedBorderStyles = css`
  border-color: ${props => props.theme.colors.tab};
`;

const StyledTabListItem = styled.li<IStyledTabListItemProp>`
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

StyledTabListItem.defaultProps = styledTabListItemDefaultProps

const StyledTabContent = styled.div`
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;

export { StyledTab, StyledTabList, StyledTabListItem, StyledTabContent };
