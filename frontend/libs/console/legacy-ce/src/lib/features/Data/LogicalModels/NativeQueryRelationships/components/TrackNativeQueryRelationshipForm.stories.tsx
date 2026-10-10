import { Meta, StoryObj } from '@storybook/react-webpack5';
import { Button, SimpleForm } from '@hasura/shared/ui';

import { ReactQueryDecorator } from '@hasura/shared/testing';
import { TrackNativeQueryRelationshipForm } from './TrackNativeQueryRelationshipForm';
import { nativeQueryRelationshipValidationSchema } from '../schema';

export default {
  component: TrackNativeQueryRelationshipForm,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof TrackNativeQueryRelationshipForm>;

export const DefaultView: StoryObj<typeof TrackNativeQueryRelationshipForm> = {
  render: () => {
    return (
      <SimpleForm
        schema={nativeQueryRelationshipValidationSchema}
        onSubmit={(data) => {
          console.log(data);
        }}
      >
        <TrackNativeQueryRelationshipForm
          name="relationship"
          fromNativeQuery="get_authors"
          nativeQueryOptions={['get_authors', 'get_articles']}
          fromFieldOptions={['field1', 'field2']}
          toFieldOptions={['field3', 'field4']}
        />

        <Button type="submit">Submit</Button>
      </SimpleForm>
    );
  },
};

// write more tests for this part
