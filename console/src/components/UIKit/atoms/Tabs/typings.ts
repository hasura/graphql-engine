export type IStyledTabListProp = Partial<ITabListDefaultProps>;

export type IStyledTabListItemProp = {
  readonly selected: boolean;
} & Partial<ITabListItemDefaultProps>;

// default Props style objects for styledTabListItem Component
export const styledTabListItemDefaultProps = {
  fontSize: 'tab',
  mr: 40,
  pb: 'sm',
  fontWeight: 'medium',
  borderBottom: 4,
  borderColor: 'transparent',
  color: 'grey.tab',
};

type ITabListItemDefaultProps = Readonly<typeof styledTabListItemDefaultProps>;

// default Props style objects for styledTabList Component
export const styledTabListDefaultProps = {
  borderBottom: 1,
  borderColor: 'grey.border',
  display: 'flex',
  justifyContent: 'flex-start',
  alignItems: 'center',
  px: 0,
};

type ITabListDefaultProps = Readonly<typeof styledTabListDefaultProps>;

type ITabsObject = {
  title: string;
  tabContent: any;
};

export interface Props {
  tabsData: Array<ITabsObject>;
  [propsName: string]: any;
}
