import { ComponentProps } from 'react';
import VersionUpdateNotification from './VersionUpdateNotification';
import VulnerableVersionNotification from './VulnerableVersionNotification';
import Notification from './Notification';

export type NotificationsListItemProps =
  | {
      kind: 'version-update';
      props: {
        latestVersion: string;
        optOutCallback: () => void;
        onClick: () => void;
      };
    }
  | {
      kind: 'security';
      props: {
        fixedVersion: string;
      };
    }
  | {
      kind: 'default';
      props: ComponentProps<typeof Notification>;
    };

const NotificationsListItem = (props: NotificationsListItemProps) => {
  switch (props.kind) {
    case 'version-update':
      return <VersionUpdateNotification {...props.props} />;
    case 'security':
      return <VulnerableVersionNotification {...props.props} />;
    default:
      return <Notification {...props.props} />;
  }
};

export default NotificationsListItem;
