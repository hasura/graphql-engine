import {
  showErrorNotification,
  ShowErrorNotificationProps,
} from '@hasura/shared/ui';
import { useInvalidateMetadata } from '../metadata';
import { HttpError } from '@hasura/shared/types';

export const useErrorNotification = () => {
  const invalidate = useInvalidateMetadata();

  return (props: ShowErrorNotificationProps) =>
    showErrorNotification({
      ...props,
      button:
        props.error instanceof HttpError &&
        props.error.data?.callToAction === 'reload-metadata'
          ? {
              label: 'Reload database metadata',
              onClick: () => {
                invalidate();
              },
            }
          : undefined,
    });
};
