import { expect } from '@storybook/jest';
import { StoryObj, Meta } from '@storybook/react';
import {
  MongoTrackCollectionModal,
  MongoTrackCollectionModalProps,
} from './MongoTrackCollectionModal';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { userEvent, within } from '@storybook/testing-library';

export default {
  component: MongoTrackCollectionModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof MongoTrackCollectionModal>;

export const Primary: StoryObj<MongoTrackCollectionModalProps> = {
  args: {
    dataSourceName: 'mongodb',
    collectionName: 'products',
    isVisible: true,
    logicalModels: [
      {
        name: 'logical_model_1',
        fields: [
          {
            name: 'id',
            type: { scalar: 'string', nullable: false },
          },
          {
            name: 'name',
            type: { scalar: 'string', nullable: true },
          },
        ],
      },
      {
        name: 'logical_model_2',
        fields: [
          {
            name: 'id',
            type: { scalar: 'string', nullable: false },
          },
        ],
      },
    ],
  },
};

Primary.play = async ({ canvasElement }: any) => {
  const canvas = within(canvasElement);

  await expect(canvas.getByLabelText('Track Collection')).toBeVisible();
  await expect(canvas.getByLabelText('Sample documents')).toBeVisible();
  await expect(canvas.getByLabelText('Existing Logical Models')).toBeVisible();
  await expect(canvas.getByLabelText('JSON Validation Schema')).toBeVisible();
  await expect(canvas.getByText('Advanced Configuration')).toBeVisible();

  // Sample document
  await expect(
    canvas.getByText('Auto-generate Logical Models based on a sample document')
  ).toBeVisible();

  const editor = await canvas.getByText('Your sample document here');

  const textArea = editor.parentElement?.parentElement?.querySelector(
    'textarea'
  ) as HTMLTextAreaElement;

  await userEvent.paste(textArea, '{ "id": "1", "name": "test" }');

  await userEvent.click(canvas.getByText('Validate'));

  await expect(await canvas.findByText('Logical Models')).toBeVisible();
  await expect(await canvas.findAllByText('products')).toHaveLength(2);
  await expect(canvas.getByText('Advanced Configuration')).toBeVisible();

  await userEvent.click((await canvas.findAllByText('products'))[1]);

  await expect(await canvas.findByText('Data Source name')).toBeVisible();
  await expect(await canvas.findByText('mongodb')).toBeVisible();

  await expect(await canvas.findAllByText('products')).toHaveLength(3);

  await expect(await canvas.findByText('Fields')).toBeVisible();

  await expect(await canvas.findAllByText('name')).toHaveLength(2);
  await expect(await canvas.findByText('type')).toBeVisible();
  await expect(await canvas.findByText('nullable')).toBeVisible();
  await expect(await canvas.findByText('array')).toBeVisible();

  await expect(await canvas.findByText('id')).toBeVisible();
  await expect(await canvas.findAllByText('string')).toHaveLength(2);
  await expect(await canvas.findAllByText('false')).toHaveLength(4);

  await userEvent.click(canvas.getByText('Back'));

  // Existing Logical Models
  await userEvent.click(canvas.getByText('Existing Logical Models'));
  await expect(await canvas.findByText('logical_model_1')).toBeVisible();

  await userEvent.selectOptions(
    canvas.getByTestId('logicalModel'),
    'logical_model_2'
  );
  await expect(await canvas.findByText('logical_model_2')).toBeVisible();

  // JSON Validation Schema
  await userEvent.click(canvas.getByText('JSON Validation Schema'));

  await expect(
    canvas.getByText('A JSON Validation Schema is required')
  ).toBeVisible();
  await expect(
    canvas.getByText(
      'Please ensure a JSON validation schema is loaded in your Collection. A JSON validation schema is required for Hasura to automatically generate a GraphQL types from your database.'
    )
  ).toBeVisible();

  await expect(canvas.getByText('Advanced Configuration')).toBeVisible();
};
