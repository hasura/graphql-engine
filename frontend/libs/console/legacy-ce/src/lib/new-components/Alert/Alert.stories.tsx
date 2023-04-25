import { ComponentMeta, ComponentStory } from '@storybook/react';
import React, { useReducer } from 'react';

import { expect, jest } from '@storybook/jest';
import { screen, userEvent, within } from '@storybook/testing-library';
import { useHasuraAlert } from '.';
import useUpdateEffect from '../../hooks/useUpdateEffect';
import { Button } from '../Button';
import { useDestructiveAlert } from './AlertProvider';

export default {
  title: 'components/Alert Dialog 🧬',
  decorators: [
    Story => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
} as ComponentMeta<any>;

/**
 *
 * Basic Alert
 *
 */

export const Alert: ComponentStory<any> = () => {
  const { hasuraAlert } = useHasuraAlert();
  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraAlert({
            message: 'This is an alert!',
            title: 'Some Title',
          });
        }}
      >
        Open an alert!
      </Button>
    </div>
  );
};
Alert.storyName = '🧰 Alert';
Alert.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Some Title')).toBeInTheDocument();
};

/**
 *
 * Basic Confirm
 *
 */

export const Confirm: ComponentStory<any> = () => {
  const { hasuraConfirm } = useHasuraAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraConfirm({
            message: 'This is a confirm!',
            title: 'Some  Title',
            onClose: ({ confirmed }) => {},
          });
        }}
      >
        Open a confirm!
      </Button>
    </div>
  );
};

Confirm.storyName = '🧰 Confirm';
Confirm.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Some Title')).toBeInTheDocument();
};

/**
 *
 * Confirm Interaction Test
 *
 * - tests if user selection of confirm/cancel is set correctly
 *
 */

export const ConfirmTest: ComponentStory<any> = () => {
  const { hasuraConfirm } = useHasuraAlert();
  const [choice, setChoice] = React.useState<'cancelled' | 'confirmed'>();
  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraConfirm({
            message: 'This is a confirm!',
            title: 'Some  Title',
            onClose: ({ confirmed }) => {
              setChoice(confirmed ? 'confirmed' : 'cancelled');
            },
          });
        }}
      >
        Open a confirm!
      </Button>
      <div>
        Your selection: <span data-testid="choice">{choice ?? ''}</span>
      </div>
    </div>
  );
};

ConfirmTest.storyName = '🧪 Confirm';
ConfirmTest.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Some Title')).toBeInTheDocument();

  await userEvent.click(await screen.findByText('Ok'));
  await expect(await screen.findByTestId('choice')).toHaveTextContent(
    'confirmed'
  );

  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Some Title')).toBeInTheDocument();

  await userEvent.click(await screen.findByText('Cancel'));
  await expect(await screen.findByTestId('choice')).toHaveTextContent(
    'cancelled'
  );
};

/**
 *
 * Basic Prompt
 *
 */

export const Prompt: ComponentStory<any> = () => {
  const { hasuraPrompt } = useHasuraAlert();
  const [choice, setChoice] = React.useState('');
  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraPrompt({
            message: 'This is a prompt',
            title: 'Some Title',
            onClose: result => {
              if (result.confirmed) {
                // discriminated union only makes result.promptValue available when user confirms
                setChoice(result.promptValue);
              } else {
                //no prompt value here.
              }
            },
          });
        }}
      >
        Open a prompt!
      </Button>
      <div className="my-4">Your value: {choice}</div>
    </div>
  );
};
Prompt.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Some Title')).toBeInTheDocument();
};
Prompt.storyName = '🧰 Prompt';

/**
 *
 * Prompt Interaction Test
 *
 * - Tests if value that user enters, and is passed to callback matches when set to component state
 *
 */
export const PromptTest: ComponentStory<any> = () => {
  const { hasuraPrompt } = useHasuraAlert();
  const [value, setValue] = React.useState('');
  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraPrompt({
            message: 'This is a prompt',
            title: 'Some Title',
            promptLabel: 'Input Label',
            onClose: result => {
              if (result.confirmed) {
                // discriminated union only makes result.promptValue available when user confirms
                setValue(result.promptValue);
              } else {
                //no prompt value here.
              }
            },
          });
        }}
      >
        Open a prompt!
      </Button>
      <div className="my-4">
        Your value: <span data-testid="prompt-value">{value}</span>
      </div>
    </div>
  );
};
PromptTest.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Some Title')).toBeInTheDocument();
  await userEvent.type(await screen.findByLabelText('Input Label'), 'blah');
  await userEvent.click(await screen.findByText('Ok'));
  await expect(await screen.findByTestId('prompt-value')).toHaveTextContent(
    'blah'
  );
};
PromptTest.storyName = '🧪 Prompt';

/**
 *
 * Confirm with custom text
 *
 */
export const CustomText: ComponentStory<any> = () => {
  const { hasuraConfirm } = useHasuraAlert();

  const [choice, setChoice] = React.useState('');
  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraConfirm({
            message: 'Which path will you choose?',
            title: 'Choose Wisely!',
            cancelText: 'Good',
            confirmText: 'Evil',
            onClose: ({ confirmed }) => {
              setChoice(!confirmed ? 'Good 😇' : 'Evil 😈');
            },
          });
        }}
      >
        Open a confirm!
      </Button>
      <div className="my-4">You chose: {choice}</div>
    </div>
  );
};

CustomText.storyName = '🎭 Variant - Custom Button Text';
CustomText.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Choose Wisely!')).toBeInTheDocument();

  await expect(await screen.findByText('Good')).toBeInTheDocument();
  await expect(await screen.findByText('Evil')).toBeInTheDocument();
};

/**
 *
 * Confirm - with destructive flag
 *
 */

export const Destructive: ComponentStory<any> = () => {
  const { hasuraConfirm } = useHasuraAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraConfirm({
            message: 'Do the risky thing?',
            title: 'Are you sure?',
            destructive: true,
            onClose: ({ confirmed }) => {
              //do something
            },
          });
        }}
      >
        Open a destructive confirm!
      </Button>
    </div>
  );
};

Destructive.storyName = '🎭 Variant - Destructive';

Destructive.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(await screen.findByText('Are you sure?')).toBeInTheDocument();

  await expect(
    screen.getByRole('button', {
      name: /Ok/i,
    })
  ).toHaveClass('text-red-600');
};
const doAsyncAction = () => {
  return new Promise<void>(res => {
    setTimeout(() => {
      res();
    }, 3000);
  });
};

/**
 *
 * Confirm - demonstrates Async Mode
 *
 */

export const AsyncMode: ComponentStory<any> = () => {
  const { hasuraConfirm } = useHasuraAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraConfirm({
            message: 'Async mode with a loading spinner',
            title: 'Async Operation',
            confirmText: 'Save Data',
            onCloseAsync: async ({ confirmed }) => {
              if (confirmed) {
                await doAsyncAction();
              }
            },
          });
        }}
      >
        Open a confirm!
      </Button>
    </div>
  );
};

AsyncMode.storyName = '🪄 Async Confirm';
AsyncMode.parameters = {
  docs: {
    description: {
      story: `#### 🚦 Usage
- Use \`onCloseAsync\` instead of \`onClose\` and return a \`Promise\`. Loading spinner will show until Promise is resolved.`,
    },
  },
};

/**
 *
 * Confirm - demonstrates Async Mode w/ optional success state
 *
 */

export const AsyncModeWithSuccess: ComponentStory<any> = () => {
  const { hasuraConfirm } = useHasuraAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraConfirm({
            message:
              'Async action with a loading spinner followed by a success indication',
            title: 'Async Operation',
            confirmText: 'Save Data',
            onCloseAsync: async ({ confirmed }) => {
              if (confirmed) {
                await doAsyncAction();
                return { withSuccess: true, successText: 'Saved!' };
              } else {
                return { withSuccess: false };
              }
            },
          });
        }}
      >
        Open a confirm!
      </Button>
    </div>
  );
};

AsyncModeWithSuccess.storyName = '🪄 Async Confirm - with success indicator';

AsyncModeWithSuccess.parameters = {
  docs: {
    description: {
      story: `#### 🚦 Usage
- Use \`onCloseAsync\` instead of \`onClose\` and return a \`Promise\`. Loading spinner will show until Promise is resolved.
- To enable a success indication, return an object from your Promise like this: \`{ withSuccess: true, successText: 'Saved!' }\``,
    },
  },
};

/**
 *
 * Prompt - demonstrates Async Mode w/ success state
 *
 */

export const AsyncPrompt: ComponentStory<any> = () => {
  const { hasuraPrompt } = useHasuraAlert();
  const [value, setValue] = React.useState('');
  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraPrompt({
            message:
              'Async action with a loading spinner followed by a success indication',
            title: 'Async Operation',
            confirmText: 'Save Data',
            onCloseAsync: async result => {
              if (result.confirmed) {
                await doAsyncAction();

                setValue(result.promptValue);

                return { withSuccess: true, successText: 'Saved!' };
              } else {
                return { withSuccess: false };
              }
            },
          });
        }}
      >
        Open a prompt!
      </Button>
      <div className="my-4">
        Your value: <span data-testid="prompt-value">{value}</span>
      </div>
    </div>
  );
};

AsyncPrompt.storyName = '🪄 Async Prompt - with success indicator';

/**
 *
 * Alert - demonstrates Async Mode w/ success state
 *
 */
export const AsyncAlert: ComponentStory<any> = () => {
  const { hasuraAlert } = useHasuraAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraAlert({
            message:
              'Async action with a loading spinner followed by a success indication',
            title: 'Async Operation',
            confirmText: 'Save Data',
            onCloseAsync: async () => {
              await doAsyncAction();

              return { withSuccess: true, successText: 'Saved!' };
            },
          });
        }}
      >
        Open an alert!
      </Button>
    </div>
  );
};

AsyncAlert.storyName = '🪄 Async Alert - with success indicator';

/**
 *
 * Async Error Handling - demonstrates built-in error handling
 *
 */
export const ErrorHandling: ComponentStory<any> = () => {
  const { hasuraAlert } = useHasuraAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraAlert({
            message:
              'This alert will throw an error during the onClose callback',
            title: 'Some Operation',
            confirmText: 'Save Data',
            onClose: () => {
              throw new Error('Whoops this was not handled!');
            },
          });
        }}
      >
        Open an alert!
      </Button>
    </div>
  );
};

ErrorHandling.storyName = '🪄 Error Handling';

/**
 *
 * Async Error Handling - demonstrates built-in error handling
 *
 */
export const AsyncErrorHandling: ComponentStory<any> = () => {
  const { hasuraAlert } = useHasuraAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          hasuraAlert({
            message: 'This alert will throw an error after a timeout',
            title: 'Async Operation',
            confirmText: 'Save Data',
            onCloseAsync: async () => {
              await doAsyncAction();
              throw new Error('Whoops this was not handled!');
            },
          });
        }}
      >
        Open an alert!
      </Button>
    </div>
  );
};

AsyncErrorHandling.storyName = '🪄 Error Handling - Async Mode';

/**
 *
 * useDestructiveConfirm - demonstrates wrapper function
 *
 */
export const DestructiveConfirm: ComponentStory<any> = () => {
  const { destructiveConfirm } = useDestructiveAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          destructiveConfirm({
            resourceName: 'My Database',
            resourceType: 'Data Source',
            destroyTerm: 'remove',
            onConfirm: async () => {
              await doAsyncAction();

              //return a boolean to indicate success:
              return true;
            },
          });
        }}
      >
        Open a destructive confirm!
      </Button>
    </div>
  );
};

DestructiveConfirm.storyName = '💥 Destructive Confirm';

DestructiveConfirm.parameters = {
  docs: {
    description: {
      story: `#### 🚦 Usage
- When needing a confirm to delete a resource, this hook standardizes the UI/UX and language.`,
    },
  },
};

/**
 *
 * useDestructivePrompt - demonstrates wrapper function
 *
 */
export const DestructivePrompt: ComponentStory<any> = () => {
  const { destructivePrompt } = useDestructiveAlert();

  return (
    <div className="w-full">
      <Button
        onClick={() => {
          destructivePrompt({
            resourceName: 'My Database',
            resourceType: 'Data Source',
            destroyTerm: 'remove',
            onConfirm: async () => {
              await doAsyncAction();

              //return a boolean to indicate success:
              return true;
            },
          });
        }}
      >
        Open an destructive prompt!
      </Button>
    </div>
  );
};

DestructivePrompt.storyName = '💥 Destructive Prompt';
DestructivePrompt.parameters = {
  docs: {
    description: {
      story: `#### 🚦 Usage
- When needing a prompt to delete a resource, this hook standardizes the UI/UX and language.`,
    },
  },
};

const logger = {
  log: (x: string) => {
    console.log(x);
  },
};

export const ReferenceStability: ComponentStory<any> = () => {
  const { hasuraAlert, hasuraConfirm, hasuraPrompt } = useHasuraAlert();

  const { destructiveConfirm, destructivePrompt } = useDestructiveAlert();

  const [, forceUpdate] = useReducer(x => x + 1, 0);

  useUpdateEffect(() => {
    logger.log('hasuraAlert reference changed');
  }, [hasuraAlert]);

  useUpdateEffect(() => {
    logger.log('hasuraConfirm reference changed');
  }, [hasuraConfirm]);

  useUpdateEffect(() => {
    logger.log('hasuraPrompt reference changed');
  }, [hasuraPrompt]);

  useUpdateEffect(() => {
    logger.log('destructiveConfirm reference changed');
  }, [destructiveConfirm]);

  useUpdateEffect(() => {
    logger.log('destructivePrompt reference changed');
  }, [destructivePrompt]);

  React.useEffect(() => {
    console.log('expected render 👍');
  });

  const twButtonStyles =
    'border-gray-900 bg-slate-100 border-solid border rounded p-2 active:bg-slate-300';

  return (
    <div className="w-full">
      <ul>
        <li>This story will automatically test referential stability.</li>
        <li>Any referential changes will be logged to the console.</li>
        <li>
          To test manually, try pressing the buttons and checking for logs in
          the console.
        </li>
      </ul>
      <div className="space-y-2 gap-2 flex flex-row">
        <button
          data-testid="force-update"
          className={twButtonStyles}
          onClick={() => {
            forceUpdate();
          }}
        >
          force render
        </button>
        <button
          data-testid="alert"
          className={twButtonStyles}
          onClick={() => {
            hasuraAlert({ title: 'Test', message: 'test' });
          }}
        >
          open alert
        </button>
        <button
          data-testid="confirm"
          className={twButtonStyles}
          onClick={() => {
            hasuraConfirm({
              title: 'Test',
              message: 'test',
              onClose: () => {},
            });
          }}
        >
          open confirm
        </button>
        <button
          data-testid="prompt"
          className={twButtonStyles}
          onClick={() => {
            hasuraPrompt({ title: 'Test', message: 'test', onClose: () => {} });
          }}
        >
          open prompt
        </button>
        <button
          data-testid="destructive-confirm"
          className={twButtonStyles}
          onClick={() => {
            destructiveConfirm({
              resourceName: 'test',
              resourceType: 'test',
              onConfirm: async () => true,
            });
          }}
        >
          open destructive confirm
        </button>
        <button
          data-testid="destructive-prompt"
          className={twButtonStyles}
          onClick={() => {
            destructivePrompt({
              resourceName: 'test',
              resourceType: 'test',
              onConfirm: async () => true,
            });
          }}
        >
          open destructive prompt
        </button>
      </div>
    </div>
  );
};

ReferenceStability.play = async ({ canvasElement }) => {
  const logSpy = jest.spyOn(logger, 'log');

  const canvas = within(canvasElement);

  //test a force render
  await userEvent.click(canvas.getByTestId('force-update'));

  await expect(logSpy).not.toHaveBeenCalled();

  //test alert call
  await userEvent.click(canvas.getByTestId('alert'));

  await userEvent.click(await screen.findByText('Ok'));

  await expect(logSpy).not.toHaveBeenCalled();

  //test confirm call
  await userEvent.click(canvas.getByTestId('confirm'));

  await userEvent.click(await screen.findByText('Ok'));

  await expect(logSpy).not.toHaveBeenCalled();

  //test prompt call
  await userEvent.click(canvas.getByTestId('prompt'));

  await userEvent.click(await screen.findByText('Ok'));

  await expect(logSpy).not.toHaveBeenCalled();

  //test destructive confirm
  await userEvent.click(canvas.getByTestId('destructive-confirm'));

  await userEvent.click(await screen.findByText('Cancel'));

  await expect(logSpy).not.toHaveBeenCalled();

  //test destructive prompt
  await userEvent.click(canvas.getByTestId('destructive-prompt'));

  await userEvent.click(await screen.findByText('Cancel'));

  await expect(logSpy).not.toHaveBeenCalled();
};
