import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta, Story } from '@storybook/react';
import { CronTriggers } from '@/features/CronTriggers';
import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { waitForElementToBeRemoved } from '@testing-library/react';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Features/Cron Triggers/components/Form',
  component: CronTriggers.Form,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof CronTriggers.Form>;

export const Create: Story = () => {
  const [showSuccessText, setShowSuccessText] = React.useState(false);
  const onSuccess = () => {
    setShowSuccessText(true);
  };
  React.useEffect(() => {
    // mock notification behaviour as our redux based notification does not work in storybook
    if (showSuccessText) {
      setTimeout(() => setShowSuccessText(false), 2000);
    }
  }, [showSuccessText]);
  return (
    <>
      <CronTriggers.Form onSuccess={onSuccess} />;
      {showSuccessText ? <p>Form saved successfully!</p> : null}
    </>
  );
};

export const Modify: Story = args => {
  return <CronTriggers.Form {...args} />;
};

Modify.args = {
  cronTriggerName: 'cron_1',
};

Create.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Click the submit button without filling up the form
  await userEvent.click(canvas.getByText('Add Cron Trigger'));

  // Step 1: Expect error messages due to required fields being empty
  expect(
    await canvas.findByText('Cron Trigger name is a required field!')
  ).toBeVisible();
  expect(
    await canvas.findByText('Webhook url is a required field!')
  ).toBeVisible();
  expect(
    await canvas.findByText('Cron Schedule is a required field!')
  ).toBeVisible();
  expect(await canvas.findByText('Payload must be valid json')).toBeVisible();

  // --------------------------------------------------
  // Step 2: Fill up name and webhook url
  await userEvent.type(
    canvas.getByRole('textbox', {
      name: 'Name',
    }),
    'test'
  );
  await userEvent.type(
    canvas.getByRole('textbox', {
      name: 'Webhook URL',
    }),
    'http://example.com'
  );

  // --------------------------------------------------
  // Step 3: Test cron schedule selector button
  await userEvent.click(canvas.getByText('Frequently used crons'));
  await userEvent.click(canvas.getByText('Every minute'));
  expect(canvas.getByRole('textbox', { name: 'Cron Schedule' })).toHaveValue(
    '* * * * *'
  );

  // --------------------------------------------------
  // Step 4: Test the JSON Payload editor

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

  // --------------------------------------------------
  // Step 5: Add request transform options
  await userEvent.click(
    canvas.getByText('Header, URL, and Advanced Request Options')
  );

  // Add request headers
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

  // Add request method and url template
  await userEvent.click(
    canvas.getByRole('radio', {
      name: 'GET',
    })
  );

  await userEvent.type(
    canvas.getByRole('textbox', {
      name: 'Request URL Template',
    }),
    '/users'
  );

  // Add request query params
  await userEvent.click(canvas.getByText('Add query params'));
  await userEvent.type(
    canvas.getByRole('textbox', {
      name: 'query_params[0].name',
    }),
    'userId'
  );
  await userEvent.type(
    canvas.getByRole('textbox', {
      name: 'query_params[0].value',
    }),
    '12'
  );

  // --------------------------------------------------
  // Step 6: Update retry configuration fields
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
  // Step 7: Save the form and assert success by observing behaviour in UI
  await userEvent.click(canvas.getByText('Add Cron Trigger'));

  // TODO: Ideally we should be checking if the success notification got fired, but our redux-based notifications does not work in storybook
  expect(await canvas.findByText('Form saved successfully!')).toBeVisible();
};
