import { Meta, StoryObj } from '@storybook/react';
import React, { useReducer } from 'react';

import { expect, jest } from '@storybook/jest';
import { screen, userEvent, waitFor, within } from '@storybook/testing-library';
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
} as Meta<any>;

export const Alert: StoryObj<any> = {
  render: () => {
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
  },

  name: '🧰 Alert',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button'));
    await expect(await screen.findByText('Some Title')).toBeInTheDocument();
  },
};

export const Confirm: StoryObj<any> = {
  render: () => {
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
  },

  name: '🧰 Confirm',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button'));
    await expect(await screen.findByText('Some Title')).toBeInTheDocument();
  },
};

export const ConfirmTest: StoryObj<any> = {
  render: () => {
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
  },

  name: '🧪 Confirm',

  play: async ({ canvasElement }) => {
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
  },
};

export const Prompt: StoryObj<any> = {
  render: () => {
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
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button'));
    await expect(await screen.findByText('Some Title')).toBeInTheDocument();
  },

  name: '🧰 Prompt',
};

export const PromptTest: StoryObj<any> = {
  render: () => {
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
          Your value: <span data-testid="prompt-value-display">{value}</span>
        </div>
      </div>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button'));
    await expect(await screen.findByText('Some Title')).toBeInTheDocument();
    await userEvent.type(await screen.findByLabelText('Input Label'), 'blah');
    await userEvent.click(await screen.findByText('Ok'));
    await waitFor(() =>
      expect(screen.getByTestId('prompt-value-display')).toHaveTextContent(
        'blah'
      )
    );
  },

  name: '🧪 Prompt',
};

export const CustomText: StoryObj<any> = {
  render: () => {
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
  },

  name: '🎭 Variant - Custom Button Text',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button'));
    await expect(await screen.findByText('Choose Wisely!')).toBeInTheDocument();

    await expect(await screen.findByText('Good')).toBeInTheDocument();
    await expect(await screen.findByText('Evil')).toBeInTheDocument();
  },
};

export const Destructive: StoryObj<any> = {
  render: () => {
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
  },

  name: '🎭 Variant - Destructive',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button'));
    await expect(await screen.findByText('Are you sure?')).toBeInTheDocument();

    await expect(
      screen.getByRole('button', {
        name: /Ok/i,
      })
    ).toHaveClass('text-red-600');
  },
};

const doAsyncAction = () => {
  return new Promise<void>(res => {
    setTimeout(() => {
      res();
    }, 3000);
  });
};

export const AsyncMode: StoryObj<any> = {
  render: () => {
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
  },

  name: '🪄 Async Confirm',

  parameters: {
    docs: {
      description: {
        story: `#### 🚦 Usage
  - Use \`onCloseAsync\` instead of \`onClose\` and return a \`Promise\`. Loading spinner will show until Promise is resolved.`,
      },
    },
  },
};

export const AsyncModeWithSuccess: StoryObj<any> = {
  render: () => {
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
  },

  name: '🪄 Async Confirm - with success indicator',

  parameters: {
    docs: {
      description: {
        story: `#### 🚦 Usage
  - Use \`onCloseAsync\` instead of \`onClose\` and return a \`Promise\`. Loading spinner will show until Promise is resolved.
  - To enable a success indication, return an object from your Promise like this: \`{ withSuccess: true, successText: 'Saved!' }\``,
      },
    },
  },
};

export const AsyncPrompt: StoryObj<any> = {
  render: () => {
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
          Your value: <span data-testid="prompt-value-display">{value}</span>
        </div>
      </div>
    );
  },

  name: '🪄 Async Prompt - with success indicator',
};

export const AsyncAlert: StoryObj<any> = {
  render: () => {
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
  },

  name: '🪄 Async Alert - with success indicator',
};

export const ErrorHandling: StoryObj<any> = {
  render: () => {
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
  },

  name: '🪄 Error Handling',
};

export const AsyncErrorHandling: StoryObj<any> = {
  render: () => {
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
  },

  name: '🪄 Error Handling - Async Mode',
};

export const DestructiveConfirm: StoryObj<any> = {
  render: () => {
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
  },

  name: '💥 Destructive Confirm',

  parameters: {
    docs: {
      description: {
        story: `#### 🚦 Usage
  - When needing a confirm to delete a resource, this hook standardizes the UI/UX and language.`,
      },
    },
  },
};

export const DestructivePrompt: StoryObj<any> = {
  render: () => {
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
  },

  name: '💥 Destructive Prompt',

  parameters: {
    docs: {
      description: {
        story: `#### 🚦 Usage
  - When needing a prompt to delete a resource, this hook standardizes the UI/UX and language.`,
      },
    },
  },
};

const logger = {
  log: (x: string) => {
    console.log(x);
  },
};

export const ReferenceStability: StoryObj<any> = {
  render: () => {
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
              hasuraPrompt({
                title: 'Test',
                message: 'test',
                onClose: () => {},
              });
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
  },

  play: async ({ canvasElement }) => {
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

    await userEvent.type(await screen.findByTestId('prompt_value'), 'blah');

    await userEvent.click(await screen.findByText('Ok'));

    await expect(logSpy).not.toHaveBeenCalled();

    //wait for alert to not be visible
    await waitFor(
      () => {
        expect(screen.queryByTestId('alert-confirm-button')).toEqual(null);
      },
      { timeout: 500 }
    );

    //test destructive confirm
    await userEvent.click(canvas.getByTestId('destructive-confirm'));

    await userEvent.click(await screen.findByText('Cancel'));

    await expect(logSpy).not.toHaveBeenCalled();

    await waitFor(
      () => {
        expect(screen.queryByTestId('alert-confirm-button')).toEqual(null);
      },
      { timeout: 500 }
    );

    //test destructive prompt
    await userEvent.click(canvas.getByTestId('destructive-prompt'));

    await userEvent.click(await screen.findByText('Cancel'));

    await expect(logSpy).not.toHaveBeenCalled();
  },
};
