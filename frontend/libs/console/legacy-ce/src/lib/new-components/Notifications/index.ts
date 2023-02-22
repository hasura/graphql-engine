import { useCallback } from 'react';
import {
  showSuccessNotification,
  showErrorNotification,
  showInfoNotification,
  showWarningNotification,
} from '../../components/Services/Common/Notification';
import { useDispatch } from 'react-redux';

// this is just a placeholder until we can revamp our notif system to react-hot-toast
export const useFireNotification = () => {
  const dispatch = useDispatch();

  const fireNotification = useCallback(
    ({
      title,
      message,
      type,
      error,
    }: {
      title: string;
      message: string;
      type: 'success' | 'error' | 'warning' | 'info';
      error?: Record<string, any>;
    }) => {
      if (type === 'error')
        dispatch(showErrorNotification(title, message, error));
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
