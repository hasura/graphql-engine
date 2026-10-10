import { StoryObj, Meta } from '@storybook/react-webpack5';
import { FormProvider, useForm } from 'react-hook-form';
import { AppTheme } from '@hasura/shared/ui';
import { MapSelector, TypeMap } from './MapSelector';
import { Schema } from '../schema';

// MapSelector reads/writes its `mapping` field through `useFormContext<Schema>()`
// (hardcoded field name "mapping"), so it must be rendered inside a
// react-hook-form `FormProvider`, matching how `FormElements.tsx` renders it.
const MapSelectorStory = ({
  mapping,
  types,
  columns,
}: {
  mapping: TypeMap[];
  types: string[];
  columns: string[];
}) => {
  const formMethods = useForm<Pick<Schema, 'mapping'>>({
    defaultValues: { mapping },
  });

  return (
    <AppTheme>
      <FormProvider {...formMethods}>
        <MapSelector
          types={types}
          columns={columns}
          mapping={mapping}
          setMapping={(values) => formMethods.setValue('mapping', values)}
        />
      </FormProvider>
    </AppTheme>
  );
};

export default {
  title: 'components/MapSelector',
  parameters: {
    docs: {
      description: {
        component: `Lets a user map a set of source "fields" to target "columns" (e.g. for remote relationships). Must be rendered inside a \`FormProvider\` (react-hook-form), since it reads/writes form state via \`useFormContext\`.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
  component: MapSelectorStory,
} as Meta<typeof MapSelectorStory>;

export const ApiPlayground: StoryObj<typeof MapSelectorStory> = {
  args: {
    types: ['id', 'name', 'email'],
    columns: ['id', 'full_name', 'email_address'],
    mapping: [{ field: 'id', column: 'id' }],
  },

  name: '⚙️ API',
};

export const Basic: StoryObj<typeof MapSelectorStory> = {
  args: {
    types: ['id', 'name', 'email'],
    columns: ['id', 'full_name', 'email_address'],
    mapping: [
      { field: 'id', column: 'id' },
      { field: 'name', column: 'full_name' },
    ],
  },

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateEmpty: StoryObj<typeof MapSelectorStory> = {
  args: {
    types: ['id', 'name', 'email'],
    columns: ['id', 'full_name', 'email_address'],
    mapping: [],
  },

  name: '🔁 State - Empty',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
