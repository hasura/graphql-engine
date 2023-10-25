import { Meta, StoryObj } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { NativeQueryRelationshipWidget } from './NativeQueryRelationshipWidget';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { handlers } from '../mocks/handlers';
import { useState } from 'react';
import { NativeQueryRelationshipFormSchema } from '../schema';

export default {
  component: NativeQueryRelationshipWidget,
  decorators: [
    ReactQueryDecorator(),
    ReduxDecorator({
      tables: {
        dataHeaders: {
          'x-hasura-admin-secret': 'myadminsecretkey' as any,
        },
      },
    }),
  ],
} as Meta<typeof NativeQueryRelationshipWidget>;

export const DefaultView: StoryObj<typeof NativeQueryRelationshipWidget> = {
  render: () => {
    return (
      <NativeQueryRelationshipWidget
        mode="create"
        fromNativeQuery="get_authors"
        dataSourceName="chinook"
      />
    );
  },
};

export const TestBasicInteraction: StoryObj<
  typeof NativeQueryRelationshipWidget
> = {
  render: () => {
    const [formValues, setFormValues] =
      useState<NativeQueryRelationshipFormSchema>();
    return (
      <div>
        <NativeQueryRelationshipWidget
          mode="create"
          fromNativeQuery="get_authors"
          dataSourceName="chinook"
          onSubmit={data => setFormValues(data)}
        />
        <div data-testid="result">{JSON.stringify(formValues)}</div>
      </div>
    );
  },
  parameters: {
    msw: handlers(),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(
      async () => {
        return await expect(
          canvas.getByLabelText('Relationship Name')
        ).toBeInTheDocument();
      },
      {
        timeout: 5000,
      }
    );

    // console.log(await canvas.getByLabelText('Relationship Name'));
    await userEvent.type(
      await canvas.getByLabelText('Relationship Name'),
      'articles'
    );

    await userEvent.selectOptions(
      await canvas.getByLabelText('Target Native Query'),
      'get_article'
    );

    await userEvent.selectOptions(
      await canvas.getByLabelText('Relationship Type'),
      'array'
    );

    await waitFor(
      async () => {
        return await expect(
          canvas.getByTestId('columnMapping_source_input_0')
        ).toBeInTheDocument();
      },
      {
        timeout: 5000,
      }
    );

    await userEvent.selectOptions(
      await canvas.getByTestId('columnMapping_source_input_0'),
      'id'
    );

    await userEvent.selectOptions(
      await canvas.getByTestId('columnMapping_target_input_0'),
      'author_id'
    );

    await userEvent.click(canvas.getByText('Add Relationship'));

    await waitFor(async () => {
      return await expect(await canvas.getByTestId('result')).toHaveTextContent(
        JSON.stringify({
          name: 'articles',
          toNativeQuery: 'get_article',
          type: 'array',
          columnMapping: { id: 'author_id' },
        })
      );
    });
  },
};
