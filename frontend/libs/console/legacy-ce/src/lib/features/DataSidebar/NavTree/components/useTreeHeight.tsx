import { useEffect, useState } from 'react';
import { SIDEBAR_ID, NON_TREE_CONTENT_IDS } from '../../constants';

const getTreeHeight = () => {
  if (process.env.STORYBOOK)
    return window.document.documentElement.scrollHeight - 250;

  const sidebarHeight = document.getElementById(SIDEBAR_ID)?.offsetHeight ?? 0;

  const otherContentHeight = NON_TREE_CONTENT_IDS.reduce((sum, val) => {
    const height = document.getElementById(val)?.offsetHeight;
    return sum + (height ?? 0);
  }, 0);

  const treeHeight = sidebarHeight - otherContentHeight - 2;

  return treeHeight;
};

// because of the way react-arborist requires a hard-coded height to draw the tree, we have to calculcate the
// available space and pass a specific value to the tree component:
export function useTreeHeight() {
  const [treeHeight, setTreeHeight] = useState(getTreeHeight());

  useEffect(() => {
    window.addEventListener('resize', () => setTreeHeight(getTreeHeight()));

    setTreeHeight(getTreeHeight());

    return () => {
      window.removeEventListener('resize', () =>
        setTreeHeight(getTreeHeight())
      );
    };
  }, []);

  return { treeHeight };
}
