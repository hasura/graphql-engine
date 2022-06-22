import { useCallback } from 'react';
import {
  showErrorNotification,
  showSuccessNotification,
  showWarningNotification,
  showInfoNotification,
} from '@/components/Services/Common/Notification';
import { useAppDispatch } from '@/store';

// this is just a placeholder until we can revamp our notif system to react-hot-toast
export const useFireNotification = () => {
  const dispatch = useAppDispatch();

  const fireNotification = useCallback(
    ({
      title,
      message,
      type,
    }: {
      title: string;
      message: string;
      type: 'success' | 'error' | 'warning' | 'info';
    }) => {
      if (type === 'error') dispatch(showErrorNotification(title, message));
      else if (type === 'success')
        dispatch(showSuccessNotification(title, message));
      else if (type === 'warning')
        dispatch(showWarningNotification(title, message));
      else dispatch(showInfoNotification(message));
    },
    [dispatch]
  );

  return { fireNotification };
};
