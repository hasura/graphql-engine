import * as React from 'react';
import { useDeleteSlackApp } from './useDeleteSlackApp';
import { useSlackOAuth } from './useSlackOAuth';

export { SlackOauthStatus } from './useSlackOAuth';

export const useSlackIntegration = (
  onDeleteClose: () => void,
  oauthString?: string
) => {
  const { slackOauthStatus, startSlackOAuth } = useSlackOAuth(oauthString);
  const { deleteSlackApp } = useDeleteSlackApp(onDeleteClose);

  return { slackOauthStatus, startSlackOAuth, deleteSlackApp };
};
