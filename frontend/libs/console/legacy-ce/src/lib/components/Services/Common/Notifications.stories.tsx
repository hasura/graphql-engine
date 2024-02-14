import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { Description, Stories, Subtitle, Title } from '@storybook/addon-docs';
import { ReduxDecorator } from '../../../storybook/decorators/redux-decorator';
import { ToastsHub } from '../../../new-components/Toasts';
import { Button } from '../../../new-components/Button';
import {
  showErrorNotification,
  showInfoNotification,
  showNotification,
  showSuccessNotification,
  showWarningNotification,
} from './Notification';
import { useAppDispatch } from '../../../storeHooks';
import { useFireNotification } from '../../../new-components/Notifications';

export default {
  title: 'components/Toasts 🧬/Legacy',
  parameters: {
    source: { type: 'code' },
    docs: {
      description: {
        component: `Those stories demonstrate the legacy way of using the notifications system in the Console codebase.`,
      },
      page: () => (
        <>
          <Title />
          <Subtitle />
          <Description />
          <Stories />
        </>
      ),
    },
  },
  decorators: [ReduxDecorator({ tables: { currentDataSource: 'default' } })],
} as Meta;

export const Intro: StoryObj<any> = {
  render: () => {
    return <div />;
  },

  name: 'ℹ️ Intro',
};

export const DirectStoreAccess: StoryObj<any> = {
  render: () => {
    const dispatch = useAppDispatch();

    const onShowErrorHandler = React.useCallback(() => {
      dispatch(
        showErrorNotification('Error', 'This is an error notification', {
          error: 'Error Label',
        })
      );
    }, [dispatch]);
    const onShowSuccessHandler = React.useCallback(() => {
      dispatch(
        showSuccessNotification(
          'Success',
          'This is a success notification',
          true
        )
      );
    }, [dispatch]);
    const onShowInfoHandler = React.useCallback(() => {
      dispatch(showInfoNotification('Info'));
    }, [dispatch]);
    const onShowWarningHandler = React.useCallback(() => {
      dispatch(
        showWarningNotification(
          'Warning',
          'This is a warning notification',
          '[Data-object]',
          <div>Some element</div>
        )
      );
    }, [dispatch]);

    return (
      <>
        <ToastsHub />
        <Button onClick={onShowErrorHandler}>
          <span>Show error notification!</span>
        </Button>
        <Button onClick={onShowSuccessHandler}>
          <span>Show success notification!</span>
        </Button>
        <Button onClick={onShowInfoHandler}>
          <span>Show info notification!</span>
        </Button>
        <Button onClick={onShowWarningHandler}>
          <span>Show warning notification!</span>
        </Button>
      </>
    );
  },

  name: '🧰  Direct Store Access',

  parameters: {
    docs: {
      source: { state: 'open' },
      description: {
        story: `This story demonstrates direct store access to trigger a notification with direct store access thanks to \`useAppDispatch\` and helper function \`showSuccessNotification\`.

  The \`showSuccessNotification\` helper function offers all presets and a curated list of props to display a success notification.

  \`showErrorNotification\`, \`showSuccessNotification\`, \`showInfoNotification\`, \`showWarningNotification,\` are also available.`,
      },
    },
  },
};

export const Hook: StoryObj<any> = {
  render: () => {
    const { fireNotification } = useFireNotification();

    const onShowErrorHandler = React.useCallback(() => {
      fireNotification({
        title: 'Error',
        message: 'This is an error notification',
        type: 'error',
      });
    }, [fireNotification]);
    const onShowSuccessHandler = React.useCallback(() => {
      fireNotification({
        title: 'Success',
        message: 'This is a success notification',
        type: 'success',
      });
    }, [fireNotification]);
    const onShowInfoHandler = React.useCallback(() => {
      fireNotification({
        title: 'Info',
        message: 'This is an info notification',
        type: 'info',
      });
    }, [fireNotification]);
    const onShowWarningHandler = React.useCallback(() => {
      fireNotification({
        title: 'Warning',
        message: 'This is a warning notification',
        type: 'warning',
      });
    }, [fireNotification]);

    return (
      <>
        <ToastsHub />
        <Button onClick={onShowErrorHandler}>
          <span>Show error notification!</span>
        </Button>
        <Button onClick={onShowSuccessHandler}>
          <span>Show success notification!</span>
        </Button>
        <Button onClick={onShowInfoHandler}>
          <span>Show info notification!</span>
        </Button>
        <Button onClick={onShowWarningHandler}>
          <span>Show warning notification!</span>
        </Button>
      </>
    );
  },

  name: '🪝 Hook',

  parameters: {
    docs: {
      source: { state: 'open' },
      description: {
        story: `This story demonstrates the use of the notification dedicated hook to trigger a notification thanks to \`useFireNotification\`.

  The \`useFireNotification\` offers all presets and a curated list of options to display any type of notification. .

  👍 The store access is hidden behind the hook.`,
      },
    },
  },
};

export const CustomCall: StoryObj<any> = {
  render: () => {
    const dispatch = useAppDispatch();

    const onClickHandler = React.useCallback(() => {
      dispatch(
        showNotification(
          {
            title: `Title!`,
            level: 'info',
            autoDismiss: 0,
            dismissible: true,
            alternateActionButtonProps: {
              label: 'View Database',
              onClick: () => console.log('Custom action'),
              trackId: 'data-tab-view-database-notification-button-add-db',
            },
          },
          'info'
        )
      );
    }, [dispatch]);

    return (
      <>
        <ToastsHub />
        <Button onClick={onClickHandler}>
          <span>Show notification!</span>
        </Button>
      </>
    );
  },

  name: '⚙️ Custom Call',

  parameters: {
    docs: {
      source: { state: 'open' },
      description: {
        story: `This story demonstrates the use of the low level custom call a notification thanks to \`useAppDispatch\` and helper function \`showNotification\`.

  The \`showNotification\` function allows to define all notification properties, including the button click handler.`,
      },
    },
  },
};
