import { useDispatch } from 'react-redux';
import _push from '../../../components/Services/Data/push';
import { useIsStorybook } from '../../../utils/StoryUtils';
import { useCallback } from 'react';

export const usePushRoute = () => {
  const dispatch = useDispatch();
  const { isStorybook } = useIsStorybook();

  const pushRoute = useCallback(
    (path: string) => {
      // if you try to push routes in storybook, it causes an error that is only recoverable by re-loading
      // usually our components shouldn't try this in storybook, but sometimes it's a side effect
      if (isStorybook) return;

      dispatch(_push(path));
    },
    [dispatch, isStorybook]
  );

  return pushRoute;
};
