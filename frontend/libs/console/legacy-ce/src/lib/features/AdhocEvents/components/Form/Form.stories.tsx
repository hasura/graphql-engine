import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { StoryObj, Meta } from '@storybook/react';
import { within, userEvent, waitFor } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { waitForElementToBeRemoved } from '@storybook/testing-library';
import { handlers } from '../../../../mocks/metadata.mock';
import { AdhocEvents } from '../..';

export default {
  title: 'Features/Scheduled Triggers/components/Form',
  component: AdhocEvents.Form,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof AdhocEvents.Form>;

export const Create: StoryObj = {
  render: () => {
    const [showSuccessText, setShowSuccessText] = React.useState(false);
    const onSuccess = () => {
      setShowSuccessText(true);
    };
    return (
      <>
        <AdhocEvents.Form onSuccess={onSuccess} />
        <p data-testid="@onSuccess">
          {showSuccessText ? 'Form saved successfully!' : null}
        </p>
      </>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Click the submit button without filling up the form
    await userEvent.click(canvas.getByText('Create scheduled event'));

    // Step 1: Expect error messages due to required fields being empty
    expect(
      await canvas.findByText('Webhook url is a required field!')
    ).toBeVisible();
    expect(await canvas.findByText('Payload must be valid json')).toBeVisible();

    // --------------------------------------------------
    // Step 2: Fill up webhook url
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'Webhook URL',
      }),
      'http://example.com'
    );

    // --------------------------------------------------
    // Step 3: Test the JSON Payload editor

    // Bug: Using class selector as ace textbox is unaccessible otherwise https://github.com/ajaxorg/ace/issues/2164
    const aceTextbox = canvasElement.getElementsByClassName(
      'ace_text-input'
    )[0] as HTMLTextAreaElement;

    // Test error message being displayed if payload type is not JSON
    // Bug: userEvent.type doesn't work in case of Ace editor https://github.com/securingsincity/react-ace/issues/923#issuecomment-1066025492
    await userEvent.paste(aceTextbox, '{"user"}');
    expect(await canvas.findByText('Payload must be valid json')).toBeVisible();

    // Fill the correct payload and expect for error to not be there
    // Bug: setSelectionRange does not work with ace, so we manually type backspace as many times we need
    await userEvent.type(
      aceTextbox,
      'a{backspace}{backspace}{backspace}{backspace}'
    );
    await userEvent.paste(aceTextbox, ' : "test" }');
    await waitForElementToBeRemoved(() =>
      canvas.queryByText('Payload must be valid json')
    );

    // Add request headers
    await userEvent.click(canvas.getByText('Advance Settings'));
    await userEvent.click(canvas.getByText('Add request headers'));
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[0].name',
      }),
      'x-hasura-name'
    );
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[0].value',
      }),
      'hasura_user'
    );

    await userEvent.click(canvas.getByText('Add request headers'));
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[1].name',
      }),
      'x-hasura-Id'
    );
    await userEvent.selectOptions(
      canvas.getByRole('combobox', {
        name: 'headers[1].type',
      }),
      'from_env'
    );
    await userEvent.type(
      canvas.getByRole('textbox', {
        name: 'headers[1].value',
      }),
      'HASURA_ENV_ID'
    );

    // --------------------------------------------------
    // Step 4: Update retry configuration fields
    await userEvent.click(canvas.getByText('Retry Configuration'));

    await userEvent.clear(
      canvas.getByRole('spinbutton', {
        name: 'num_retries',
      })
    );
    await userEvent.type(
      canvas.getByRole('spinbutton', {
        name: 'num_retries',
      }),
      '12'
    );

    await userEvent.clear(
      canvas.getByRole('spinbutton', {
        name: 'retry_interval_seconds',
      })
    );
    await userEvent.type(
      canvas.getByRole('spinbutton', {
        name: 'retry_interval_seconds',
      }),
      '100'
    );

    await userEvent.clear(
      canvas.getByRole('spinbutton', {
        name: 'timeout_seconds',
      })
    );
    await userEvent.type(
      canvas.getByRole('spinbutton', {
        name: 'timeout_seconds',
      }),
      '20'
    );

    // --------------------------------------------------
    // Step 5: Save the form and assert success by observing behaviour in UI
    await userEvent.click(canvas.getByText('Create scheduled event'));

    // TODO: Ideally we should be checking if the success notification got fired, but our redux-based notifications does not work in storybook
    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('@onSuccess')).toHaveTextContent(
          'Form saved successfully!'
        );
      },
      { timeout: 5000 }
    );
  },
};
