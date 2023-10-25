export type SidebarLinkType = {
  to: string;
  name: string;
  icon: JSX.Element;
  hideIfNoSources?: boolean;
  cliOnly?: boolean;
  isLinkActive: () => boolean;
};
