import { getPathRoot } from '@hasura/shared/utils';

export type IsBlockActiveArgs = {
  blockPath: string;
  pathname: string;
  isDefaultBlock: boolean;
};

export const isBlockActive = ({
  blockPath,
  isDefaultBlock,
  pathname,
}: IsBlockActiveArgs) => {
  const currentActiveBlock = getPathRoot(pathname);

  // return true for defaultBlock when on console '/' path
  if (isDefaultBlock && currentActiveBlock === '') {
    return true;
  }

  const block = getPathRoot(blockPath);

  return currentActiveBlock === block;
};
