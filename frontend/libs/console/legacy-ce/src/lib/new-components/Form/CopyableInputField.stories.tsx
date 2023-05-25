import { action } from '@storybook/addon-actions';
import { Meta, StoryObj } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import { FaAirFreshener, FaBabyCarriage } from 'react-icons/fa';
import { z } from 'zod';
import { CopyableInputField as InputField, SimpleForm } from '.';

type StoryType = StoryObj<typeof InputField>;

export default {
  title: 'components/Forms 📁/CopyableInputField 🧬',
  component: InputField,
  //argTypes: { onCopy: { action: 'copied' } },
  parameters: {
    docs: {
      description: {
        component: `A component wrapping <InputField /> that allows copying of input text`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof InputField>;

export const Basic: StoryType = {
  render: args => {
    const validationSchema = z.object({});
    const [clipboardText, setClipboardText] = React.useState('');
    const [show, setShow] = React.useState(false);

    return (
      <div className="max-w-lg">
        <SimpleForm
          schema={validationSchema}
          options={{
            defaultValues: {
              demo: 'Click the copy button to copy this to the clipboard!',
            },
          }}
          onSubmit={action('onSubmit')}
        >
          <InputField
            onCopy={v => {
              void navigator.clipboard.readText().then(t => {
                setClipboardText(t);
                setShow(true);
              });
            }}
            {...args}
            name="demo"
          />
        </SimpleForm>
        <div>Current Clipboard Text: </div>
        <div className="text-muted text-sm">(Click the copy button!)</div>
        {show && (
          <div className="bg-violet-200 rounded p-3">{clipboardText}</div>
        )}
      </div>
    );
  },

  args: {
    name: 'input',
    label: 'An input field with copy button',
    placeholder: 'Type something and then use the button to copy it!',
  },

  name: 'Basic Usage',
};

export const MoreExamples: StoryType = {
  render: () => {
    const [clipboardText, setClipboardText] = React.useState('');
    const [show, setShow] = React.useState(false);
    const validationSchema = z.object({
      enabled: z.string().optional(),
      disabled: z.string().optional(),
    });

    return (
      <div className="max-w-lg">
        <SimpleForm
          schema={validationSchema}
          options={{
            defaultValues: {
              disabled:
                'npm run my-awesome-action --with some-arg --complicated yes',
            },
          }}
          onSubmit={action('onSubmit')}
        >
          <InputField
            onCopy={v => {
              void navigator.clipboard.readText().then(t => {
                setClipboardText(t);
                setShow(true);
              });
            }}
            prependLabel={'A Label, Prepended'}
            description="A showcase of this feature playing nice with existing input features"
            icon={<FaBabyCarriage />}
            labelIcon={<FaAirFreshener />}
            learnMoreLink={'https://www.google.com/search?q=learn+more'}
            tooltip={'A very useful tip!'}
            name="enabled"
            label="ALL THE OPTIONS!"
          />
          <div className="mb-4">
            <span className="font-bold">Note:</span> The following props are not
            available on this component as they create UI conflicts/issues:
            <pre>appendLabel, clearButton, iconPosition, size</pre>
          </div>
          <InputField
            onCopy={v => {
              action('onCopy');
              void navigator.clipboard.readText().then(t => {
                setClipboardText(t);
                setShow(true);
              });
            }}
            name="disabled"
            disabled
            label="Set field to disabled to present non-editable text that a user can copy"
          />
          <div>Current Clipboard Text: </div>
          <div className="text-muted text-sm">(Try copying some text!)</div>
          {show && (
            <div className="bg-violet-200 rounded p-3">{clipboardText}</div>
          )}
        </SimpleForm>
      </div>
    );
  },
};

export const Testing: StoryType = {
  render: args => {
    const validationSchema = z.object({});
    const [clipboardText, setClipboardText] = React.useState('');
    const [show, setShow] = React.useState(false);

    return (
      <div className="max-w-lg">
        <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
          <InputField
            onCopy={v => {
              void navigator.clipboard.readText().then(t => {
                setClipboardText(t);
                setShow(true);
              });
            }}
            {...args}
            name="demo"
          />
        </SimpleForm>
        <div>Current Clipboard Text: </div>
        <div className="text-muted text-sm">(Click the copy button!)</div>
        {show && (
          <div
            className="bg-violet-200 rounded p-3"
            data-testid="clipboard-contents"
          >
            {clipboardText}
          </div>
        )}
      </div>
    );
  },

  args: {
    name: 'input',
    label: 'An input field with copy button',
    placeholder: 'Type something and then use the button to copy it!',
  },

  name: 'Tests',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const testValue = 'You can copy this text to the clipboard!';

    const input = await canvas.findByTestId('demo');

    await userEvent.type(input, testValue);

    await userEvent.click(await canvas.findByTestId('copy-button'));

    // const clipboardText = await navigator.clipboard.readText();
    // await waitFor(async () => {
    //   expect(await canvas.findByTestId('clipboard-contents')).toHaveTextContent(
    //     testValue
    //   );
    // });
  },
};
