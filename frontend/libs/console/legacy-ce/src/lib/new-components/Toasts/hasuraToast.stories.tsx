import React from 'react';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import AceEditor from 'react-ace';
import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

import { hasuraToast } from '@/new-components/Toasts';
import { Button } from '@/new-components/Button';

export default {
  title: 'components/Toasts 🚧/Proposal',
  parameters: {
    docs: {
      description: {
        component: `A component wrapping the \`react-hot-toast\` library to easily implement a toast system.`,
      },
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [Story => <div className="p-4 ">{Story()}</div>],
} as ComponentMeta<any>;

export const Basic: ComponentStory<any> = () => {
  return (
    <Button
      onClick={() =>
        hasuraToast({
          title: 'The toast title',
          message: 'The toast message',
        })
      }
    >
      <span>Add toast!</span>
    </Button>
  );
};
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
Basic.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByRole('button'));
  await expect(canvas.getByText('The toast title')).toBeInTheDocument();
};

export const VariantType: ComponentStory<any> = () => {
  return (
    <>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'error',
            title: 'The toast title',
            message: 'The toast message',
          })
        }
      >
        <span>Add error toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'success',
            title: 'The toast title',
            message: 'The toast message',
          })
        }
      >
        <span>Add success toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'info',
            title: 'The toast title',
            message: 'The toast message',
          })
        }
      >
        <span>Add info toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'warning',
            title: 'The toast title',
            message: 'The toast message',
          })
        }
      >
        <span>Add warning toast!</span>
      </Button>
    </>
  );
};
VariantType.storyName = '🎭 Variant - Type';
VariantType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantButton: ComponentStory<any> = () => {
  return (
    <>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'error',
            title: 'The toast title',
            message: 'The toast message',
            button: {
              label: 'The toast button',
              onClick: action('Button clicked!'),
            },
          })
        }
      >
        <span>Add error toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'success',
            title: 'The toast title',
            message: 'The toast message',
            button: {
              label: 'The toast button',
              onClick: action('Button clicked!'),
            },
          })
        }
      >
        <span>Add success toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'info',
            title: 'The toast title',
            message: 'The toast message',
            button: {
              label: 'The toast button',
              onClick: action('Button clicked!'),
            },
          })
        }
      >
        <span>Add info toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'warning',
            title: 'The toast title',
            message: 'The toast message',
            button: {
              label: 'The toast button',
              onClick: action('Button clicked!'),
            },
          })
        }
      >
        <span>Add warning toast!</span>
      </Button>
    </>
  );
};
VariantButton.storyName = '🎭 Variant - Button';
VariantButton.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantChildren: ComponentStory<any> = () => {
  return (
    <>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'error',
            title: 'The toast title',
            message: 'The toast message',
            children: (
              <div className="overflow-hidden">
                Here is an embedded code:
                <AceEditor
                  theme="github"
                  setOptions={{
                    minLines: 3,
                    maxLines: Infinity,
                    showGutter: false,
                    useWorker: false,
                  }}
                  value={`{
  "key": "value",
}`}
                />
              </div>
            ),
            toastOptions: {
              duration: 1000000,
            },
          })
        }
      >
        <span>Add error toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'success',
            title: 'The toast title',
            message: 'The toast message',
            children: (
              <div className="overflow-hidden">
                Here is an embedded code:
                <AceEditor
                  theme="github"
                  setOptions={{
                    minLines: 3,
                    maxLines: Infinity,
                    showGutter: false,
                    useWorker: false,
                  }}
                  value={`{
  "key": "value",
}`}
                />
              </div>
            ),
          })
        }
      >
        <span>Add success toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'info',
            title: 'The toast title',
            message: 'The toast message',
            children: (
              <div className="overflow-hidden">
                Here is an embedded code:
                <AceEditor
                  theme="github"
                  setOptions={{
                    minLines: 3,
                    maxLines: Infinity,
                    showGutter: false,
                    useWorker: false,
                  }}
                  value={`{
  "key": "value",
}`}
                />
              </div>
            ),
          })
        }
      >
        <span>Add info toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'warning',
            title: 'The toast title',
            message: 'The toast message',
            children: (
              <div className="overflow-hidden">
                Here is an embedded code:
                <AceEditor
                  theme="github"
                  setOptions={{
                    minLines: 3,
                    maxLines: Infinity,
                    showGutter: false,
                    useWorker: false,
                  }}
                  value={`{
  "key": "value",
}`}
                />
              </div>
            ),
          })
        }
      >
        <span>Add warning toast!</span>
      </Button>
    </>
  );
};
VariantChildren.storyName = '🎭 Variant - Children';
VariantChildren.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantToastOptions: ComponentStory<any> = () => {
  return (
    <>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'error',
            title: 'The toast title',
            message: 'The toast message',
            toastOptions: {
              style: {
                minWidth: 'min(350px, 75vw)',
              },
            },
          })
        }
      >
        <span>Add error toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'success',
            title: 'The toast title',
            message: 'The toast message',
            toastOptions: {
              style: {
                minWidth: 'min(150px, 75vw)',
              },
            },
          })
        }
      >
        <span>Add success toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'info',
            title: 'The toast title',
            message: 'The toast message',
            toastOptions: {
              style: {
                minWidth: 'min(250px, 75vw)',
              },
            },
          })
        }
      >
        <span>Add info toast!</span>
      </Button>
      <Button
        onClick={() =>
          hasuraToast({
            type: 'warning',
            title: 'The toast title',
            message: 'The toast message',
            toastOptions: {
              style: {
                minWidth: 'min(300px, 75vw)',
              },
            },
          })
        }
      >
        <span>Add warning toast!</span>
      </Button>
    </>
  );
};
VariantToastOptions.storyName = '🎭 Variant - Toast options';
VariantToastOptions.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
