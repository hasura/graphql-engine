import { BiCodeBlock, BiCodeCurly, BiData } from 'react-icons/bi';
import { TbDatabaseExport } from 'react-icons/tb';
import { SidebarLinkType } from './types';

export const DatabaseIcon = BiData;

export const Links: readonly SidebarLinkType[] = [
  { to: '/data/manage', name: 'Manage Data', icon: <DatabaseIcon /> },
  {
    to: '/data/sql',
    name: 'SQL',
    icon: <BiCodeBlock />,
    hideIfNoSources: true,
  },
  {
    to: '/data/native-queries',
    name: 'Native Queries',
    icon: <BiCodeCurly />,
    hideIfNoSources: true,
  },
  {
    to: '/data/migrations',
    name: 'Migrations',
    icon: <TbDatabaseExport />,
    hideIfNoSources: true,
    cliOnly: true,
  },
] as const;

export const SIDEBAR_ID = 'nav-sidebar';
export const SIDEBAR_LINKS_ID = 'nav-links';
export const DATABASE_HEADER_ID = 'nav-databases-header';
export const DATABASE_SEARCH_INPUT_ID = 'nav-search';

// this is the group of id's for content that's not the actual react-arborist tree element.
// used for sizing
export const NON_TREE_CONTENT_IDS: readonly string[] = [
  SIDEBAR_LINKS_ID,
  DATABASE_HEADER_ID,
  DATABASE_SEARCH_INPUT_ID,
];
