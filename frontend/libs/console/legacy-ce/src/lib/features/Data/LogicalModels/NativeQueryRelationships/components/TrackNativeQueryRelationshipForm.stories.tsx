import { Meta, StoryObj } from '@storybook/react';
import { Button } from '../../../../../new-components/Button';
import { SimpleForm } from '../../../../../new-components/Form';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { TrackNativeQueryRelationshipForm } from './TrackNativeQueryRelationshipForm';
import { nativeQueryRelationshipValidationSchema } from '../schema';

export default {
  component: TrackNativeQueryRelationshipForm,
  decorators: [
    ReactQueryDecorator(),
    ReduxDecorator({
      tables: {
        // dataHeaders: {
        //   'x-hasura-admin-secret': 'myadminsecretkey',
        // } as any,
      },
    }),
  ],
} as Meta<typeof TrackNativeQueryRelationshipForm>;

export const DefaultView: StoryObj<typeof TrackNativeQueryRelationshipForm> = {
  render: () => {
    return (
      <SimpleForm
        schema={nativeQueryRelationshipValidationSchema}
        onSubmit={data => {
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
