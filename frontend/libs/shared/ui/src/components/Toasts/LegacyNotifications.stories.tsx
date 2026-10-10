import { StoryObj, Meta } from '@storybook/react-webpack5';
import {
  showErrorNotification,
  showSuccessNotificationLegacy,
  showInfoNotificationLegacy,
} from './legacyNotifications';
import { Button } from '../Button';

export default {
  title: 'components/Toasts 🧬/Legacy with new API',
  parameters: {
    docs: {
      description: {
        component: `A component wrapping thenew notification API to easily migrate existing notifications.`,
      },
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [(Story) => <div className="p-4 ">{Story()}</div>],
} as Meta<any>;

export const Success: StoryObj<any> = {
  render: () => {
    return (
      <>
        <Button
          onClick={() =>
            showSuccessNotificationLegacy(
              'This toast will be automatically closed in 3sec',
              'The toast message',
            )
          }
        >
          <span>Add success notification!</span>
        </Button>
        <Button
          onClick={() =>
            showSuccessNotificationLegacy(
              'This toast will not be auto closed',
              'The toast message',
              true,
            )
          }
        >
          <span>Add success notification with no autoclose!</span>
        </Button>
      </>
    );
  },

  name: '🟢 Success',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const Error: StoryObj<any> = {
  render: () => {
    return (
      <>
        <Button
          onClick={() =>
            showErrorNotification({
              title: 'The toast message',
              error: 'This toast displays a simple error',
            })
          }
        >
          <span>Add error notification!</span>
        </Button>
        <Button
          onClick={() =>
            showErrorNotification({
              message: 'This toast displays an error with more info',
              title: 'The toast message',
              error: {
                code: 'invalid-configuration',
                error: 'Inconsistent object: connection error',
                internal: [
                  {
                    definition: 'as',
                    message:
                      'missing "=" after "as" in connection info string\n',
                    name: 'source as',
                    reason: 'Inconsistent object: connection error',
                    type: 'source',
                  },
                ],
                path: '$.args[0].args',
              },
            })
          }
        >
          <span>Add error notification with no autoclose!</span>
        </Button>
      </>
    );
  },

  name: '🔴 Error',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const Info: StoryObj<any> = {
  render: () => {
    return (
      <Button
        onClick={() =>
          showInfoNotificationLegacy(
            'This toast will be automatically closed in 6sec',
            'The toast message',
          )
        }
      >
        <span>Add info notification!</span>
      </Button>
    );
  },

  name: '🔵 Info',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
