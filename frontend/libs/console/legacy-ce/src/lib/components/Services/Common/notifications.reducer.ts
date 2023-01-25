export type NotificationsAction = {
  type?:
    | 'RNS_SHOW_NOTIFICATION'
    | 'RNS_HIDE_NOTIFICATION'
    | 'RNS_REMOVE_ALL_NOTIFICATIONS';
};

export default function Notifications(
  state = [],
  action: NotificationsAction = {}
) {
  switch (action.type) {
    case 'RNS_SHOW_NOTIFICATION':
      return state;
    case 'RNS_HIDE_NOTIFICATION':
      return state;
    case 'RNS_REMOVE_ALL_NOTIFICATIONS':
      return state;
    default:
      return state;
  }
}
