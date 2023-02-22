import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';

import {
  showErrorNotificationLegacy,
  showSuccessNotificationLegacy,
  showWarningNotificationLegacy,
  showInfoNotificationLegacy,
} from '.';
import { Button } from '../Button';
import { HasuraLogoFull } from '../HasuraLogo';

export default {
  title: 'components/Toasts 🚧/Legacy with new API',
  parameters: {
    docs: {
      description: {
        component: `A component wrapping thenew notification API to easily migrate existing notifications.`,
      },
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [Story => <div className="p-4 ">{Story()}</div>],
} as ComponentMeta<any>;

export const Success: ComponentStory<any> = () => {
  return (
    <>
      <Button
        onClick={() =>
          showSuccessNotificationLegacy(
            'This toast will be automatically closed in 3sec',
            'The toast message'
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
            true
          )
        }
      >
        <span>Add success notification with no autoclose!</span>
      </Button>
    </>
  );
};
Success.storyName = '🟢 Success';
Success.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const Error: ComponentStory<any> = () => {
  return (
    <>
      <Button
        onClick={() =>
          showErrorNotificationLegacy(
            'This toast displays a simple error',
            'The toast message'
          )
        }
      >
        <span>Add error notification!</span>
      </Button>
      <Button
        onClick={() =>
          showErrorNotificationLegacy(
            'This toast displays an error with more info',
            'The toast message',
            {
              code: 'invalid-configuration',
              error: 'Inconsistent object: connection error',
              internal: [
                {
                  definition: 'as',
                  message: 'missing "=" after "as" in connection info string\n',
                  name: 'source as',
                  reason: 'Inconsistent object: connection error',
                  type: 'source',
                },
              ],
              path: '$.args[0].args',
            }
          )
        }
      >
        <span>Add error notification with no autoclose!</span>
      </Button>
    </>
  );
};
Error.storyName = '🔴 Error';
Error.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const Warning: ComponentStory<any> = () => {
  return (
    <>
      <Button
        onClick={() =>
          showWarningNotificationLegacy(
            'This toast will be automatically closed in 6sec',
            'The toast message'
          )
        }
      >
        <span>Add warning notification!</span>
      </Button>
      <Button
        onClick={() =>
          showWarningNotificationLegacy(
            'This toast will be automatically closed in 6sec',
            'The toast message',
            'More informations as code',
            <HasuraLogoFull />
          )
        }
      >
        <span>Add warning notification with more info & children!</span>
      </Button>
    </>
  );
};
Warning.storyName = '🟠 Warning';
Warning.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const Info: ComponentStory<any> = () => {
  return (
    <Button
      onClick={() =>
        showInfoNotificationLegacy(
          'This toast will be automatically closed in 6sec',
          'The toast message'
        )
      }
    >
      <span>Add info notification!</span>
    </Button>
  );
};
Info.storyName = '🔵 Info';
Info.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
