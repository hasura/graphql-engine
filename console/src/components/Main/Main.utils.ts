import { getPathRoot } from '../Common/utils/urlUtils';

type IsBlockActiveArgs = {
  blockPath: string;
  pathname: string;
  isDefaultBlock: boolean;
};

export const isBlockActive = ({
  blockPath,
  isDefaultBlock,
  pathname,
}: IsBlockActiveArgs) => {
  const block = getPathRoot(blockPath);
  const currentActiveBlock = getPathRoot(pathname);

  return (
    currentActiveBlock === block ||
    (isDefaultBlock && currentActiveBlock === '')
  );
};
