import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { RowPermissionsInput } from './RowPermissionsInput';
import { within } from '@storybook/testing-library';
import {
  fireEvent,
  userEvent,
  waitForElementToBeRemoved,
} from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import {
  tables,
  tableWithGeolocationSupport,
} from './__tests__/fixtures/tables';
import { comparators } from './__tests__/fixtures/comparators';
import { usePermissionTables } from '../hooks/usePermissionTables';
import { usePermissionComparators } from '../hooks/usePermissionComparators';
import { handlers } from './__tests__/fixtures/jsonb/handlers';
import { ReactQueryDecorator } from '../../../../../../storybook/decorators/react-query';
import isEmpty from 'lodash/isEmpty';
import { useState } from 'react';
import { Permissions } from './types';

export default {
  title: 'Features/Permissions/Form/Row Permissions Input',
  component: RowPermissionsInput,
  parameters: {
    msw: handlers(),
  },
  decorators: [ReactQueryDecorator()],
} as Meta;

export const SetRootLevelPermission: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('-operator'));
    await userEvent.selectOptions(canvas.getByTestId('-operator'), 'Subject');
  },
};

export const SetExistsPermission: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(canvas.getByTestId('-operator'), '_exists');

    await userEvent.selectOptions(
      canvas.getByTestId('_exists._table-value-input'),
      'Label'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_exists._where-operator'),
      'id'
    );

    await userEvent.type(
      canvas.getByTestId('_exists._where.id._eq-value-input'),
      '1337'
    );

    expect(canvas.getByTestId('_exists._where.id._eq-value-input')).toHaveValue(
      '1337'
    );
  },
};

export const SetMultilevelExistsPermission: StoryObj<
  typeof RowPermissionsInput
> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(canvas.getByTestId('-operator'), '_exists');

    await userEvent.selectOptions(
      canvas.getByTestId('_exists._table-value-input'),
      'Label'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_exists._where-operator'),
      '_exists'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_exists._where._exists._table-value-input'),
      'Label'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_exists._where._exists._where-operator'),
      'id'
    );

    await userEvent.type(
      canvas.getByTestId('_exists._where._exists._where.id._eq-value-input'),
      '1337'
    );

    expect(
      canvas.getByTestId('_exists._where._exists._where.id._eq-value-input')
    ).toHaveValue('1337');
  },
};

export const SetAndPermission: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(canvas.getByTestId('-operator'), '_and');

    await userEvent.selectOptions(
      canvas.getByTestId('_and.1-operator'),
      'Series_reference'
    );

    await userEvent.type(
      canvas.getByTestId('_and.1.Series_reference._eq-value-input'),
      '1337',
      { delay: 300 }
    );

    expect(
      canvas.getByTestId('_and.1.Series_reference._eq-value-input')
    ).toHaveValue('1337');
  },
};

export const SetMultilevelAndPermission: StoryObj<typeof RowPermissionsInput> =
  {
    render: args => (
      <RowPermissionsInput
        onPermissionsChange={action('onPermissionsChange')}
        table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={{}}
      />
    ),

    play: async ({ canvasElement }) => {
      const canvas = within(canvasElement);

      await userEvent.selectOptions(canvas.getByTestId('-operator'), '_and');

      await userEvent.selectOptions(
        canvas.getByTestId('_and.1-operator'),
        'Series_reference'
      );

      await userEvent.type(
        canvas.getByTestId('_and.1.Series_reference._eq-value-input'),
        '1337',
        { delay: 300 }
      );

      await userEvent.selectOptions(
        canvas.getByTestId('_and.2-operator'),
        'STATUS'
      );
      await userEvent.type(
        canvas.getByTestId('_and.2.STATUS._eq-value-input'),
        '1338'
      );

      expect(canvas.getByTestId('_and.2.STATUS._eq-value-input')).toHaveValue(
        '1338'
      );
    },
  };

export const SetNotPermission: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(canvas.getByTestId('-operator'), '_not');

    const all = await canvas.getAllByTestId('_not-operator');
    await userEvent.selectOptions(all[all?.length - 1], 'Period');

    const element = await canvas.getByLabelText('_not.Period._eq-comparator');
    await expect(element.getAttribute('id')).toEqual(
      '_not.Period._eq-comparator-select-value'
    );
  },
};

export const SetOrPermission: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(canvas.getByTestId('-operator'), '_or');

    await userEvent.selectOptions(
      canvas.getByTestId('_or.1-operator'),
      'Period'
    );

    await userEvent.type(
      canvas.getByTestId('_or.1.Period._eq-value-input'),
      '1337'
    );

    expect(canvas.getByTestId('_or.1.Period._eq-value-input')).toHaveValue(
      '1337'
    );
  },
};

export const SetMultilevelOrPermission: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(canvas.getByTestId('-operator'), '_or');

    await userEvent.selectOptions(
      canvas.getByTestId('_or.1-operator'),
      'Series_reference'
    );

    await userEvent.type(
      canvas.getByTestId('_or.1.Series_reference._eq-value-input'),
      '1337',
      { delay: 300 }
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_or.2-operator'),
      'STATUS'
    );
    await userEvent.type(
      canvas.getByTestId('_or.2.STATUS._eq-value-input'),
      '1338',
      {
        delay: 300,
      }
    );
  },
};

export const Empty: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{}}
    />
  ),
};

export const Exists: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _exists: {
          _table: {},
          _where: {},
        },
      }}
    />
  ),
};

export const SetDisabledExistsPermission: StoryObj<typeof RowPermissionsInput> =
  {
    render: args => (
      <RowPermissionsInput
        onPermissionsChange={action('onPermissionsChange')}
        table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={{
          _exists: {
            _table: {},
            _where: {},
          },
        }}
      />
    ),

    play: async ({ canvasElement }) => {
      const canvas = within(canvasElement);

      const existElement = canvas.getByTestId('_exists._where-value-input');
      expect(existElement).toHaveAttribute('disabled');

      await userEvent.selectOptions(
        canvas.getByTestId('_exists._table-value-input'),
        'Label'
      );

      expect(existElement).not.toHaveAttribute('disabled');
    },
  };

export const ExistsWhere: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _exists: {
          _table: { dataset: 'bigquery_sample', name: 'sample_table' },
          _where: {
            _and: [
              { STATUS: { _eq: 'X-Hasura-User-Id' } },
              { Period: { _eq: 'Period' } },
            ],
          },
        },
      }}
    />
  ),
};

export const EmptyExists: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _exists: {
          _table: {},
          _where: {},
        },
      }}
    />
  ),
};

export const And: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _and: [
          { STATUS: { _eq: 'X-Hasura-User-Id' } },
          { Period: { _eq: 'Period' } },
        ],
      }}
    />
  ),
};

export const EmptyAnd: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _and: [{}],
      }}
    />
  ),
};

export const Not: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _not: { STATUS: { _eq: 'X-Hasura-User-Id' } },
      }}
    />
  ),
};

export const EmptyNot: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _not: {},
      }}
    />
  ),
};

export const Relationships: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={['Album']}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{ Author: { name: { _eq: '' } } }}
    />
  ),
};

export const RelationshipsColumns: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={['Album']}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{ Label: { id: { _eq: '' } } }}
    />
  ),
};

export const ColumnTypes: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{ Series_reference: { _eq: '' } }}
    />
  ),
};

export const BooleanArrayType: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={['Album']}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{ Author: { _ceq: ['name'] } }}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByTestId('Author-operator')).toBeInTheDocument();
    const element = await canvas.getByLabelText('Author._ceq-comparator');
    await expect(element.getAttribute('id')).toEqual(
      'Author._ceq-comparator-select-value'
    );
    expect(
      canvas.getByTestId('Author._ceq-column-comparator-entry')
    ).toBeInTheDocument();
  },
};

export const BooleanArrayTypeRoot: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={['Album']}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        Author: {
          _ceq: [
            // title belongs to Root table (Album)
            'title',
            '$',
          ],
        },
      }}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    expect(
      canvas.getByTestId('Author._ceq-column-comparator-entry')
    ).toHaveValue('$');

    fireEvent.click(canvas.getByTestId('Author._ceq-column-comparator-entry'));

    // Author._ceq-column-comparator-entry shows currently selected table columns (id, name, and surname)
    expect(
      canvas.getByTestId('Author._ceq-column-comparator-entry-id')
    ).toBeInTheDocument();
    expect(
      canvas.getByTestId('Author._ceq-column-comparator-entry-name')
    ).toBeInTheDocument();
    expect(
      canvas.getByTestId('Author._ceq-column-comparator-entry-surname')
    ).toBeInTheDocument();

    fireEvent.click(
      canvas.getByTestId('Author._ceq-root-column-comparator-entry')
    );

    // Author._ceq-root-column-comparator-entry shows root columns (id and title)
    expect(
      canvas.getByTestId('Author._ceq-root-column-comparator-entry-id')
    ).toBeInTheDocument();
    expect(
      canvas.getByTestId('Author._ceq-root-column-comparator-entry-title')
    ).toBeInTheDocument();
  },
};

export const StringObjectType: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ name: 'user_location', schema: 'public' }}
      tables={tableWithGeolocationSupport}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        location: {
          _st_d_within: {
            distance: 100000,
            from: { coordinates: [1.4, 2.5], type: 'Point' },
            use_spheroid: false,
          },
        },
      }}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Test that it is handled like a string input field i.e. not creating input fields recursively
    const input = await canvas.getByTestId('location._st_d_within-value-input');
    expect(input).toHaveValue(
      '{"distance":100000,"from":{"coordinates":[1.4,2.5],"type":"Point"},"use_spheroid":false}'
    );
  },
};

export const NumericValue: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={['Album']}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{ id: { _eq: '' } }}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(
      canvas.getByTestId('id._eq-value-input-x-hasura-user-id')
    ).toBeInTheDocument();
  },
};

export const NumericIntValue: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={['Album']}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{ id: { _eq: 0 } }}
    />
  ),
};

export const NumericFloatValue: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={['Album']}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{ id: { _eq: 0.9 } }}
    />
  ),
};

export const JsonbColumns: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const { tables } = usePermissionTables({
      dataSourceName: 'default',
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={action('onPermissionsChange')}
        table={{ schema: 'public', name: 'Stuff' }}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={{ jason: { _contained_in: { a: 'b' } } }}
      />
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Wait until Loading is gone
    await waitForElementToBeRemoved(() => canvas.queryByText('Loading'), {
      timeout: 50000,
    });
    // Expect jason._contained_in-comparator to be in the document
    const element = await canvas.getByLabelText(
      'jason._contained_in-comparator'
    );
    await expect(element.getAttribute('id')).toEqual(
      'jason._contained_in-comparator-select-value'
    );
    // Expect jason._contained_in-value-input to have value "{"a": "b"}"
    expect(canvas.getByTestId('jason._contained_in-value-input')).toHaveValue(
      '{"a":"b"}'
    );
  },
};

export const JsonbColumnsHasKeys: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const { tables } = usePermissionTables({
      dataSourceName: 'default',
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={action('onPermissionsChange')}
        table={{ schema: 'public', name: 'Stuff' }}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={{ jason: { _has_keys_all: [''] } }}
      />
    );
  },
};

export const StringColumns: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const [permissions, setPermissions] = useState<Permissions>({
      name: { _eq: '' },
    });
    const { tables } = usePermissionTables({
      dataSourceName: 'default',
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={p => {
          setPermissions(p);
          args.onPermissionsChange?.(p);
        }}
        table={{ schema: 'public', name: 'Stuff' }}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={permissions}
      />
    );
  },

  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    // Wait until Loading is gone
    await waitForElementToBeRemoved(() => canvas.queryByText('Loading'), {
      timeout: 5000,
    });

    // Write a number in the input
    await userEvent.type(canvas.getByTestId('name._eq-value-input'), '1337');

    expect(args.onPermissionsChange).toHaveBeenCalledWith({
      name: {
        _eq: 1337,
      },
    });
  },
};

export const NumberColumns: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const [permissions, setPermissions] = useState<Permissions>({
      id: { _eq: '1234' },
    });
    const { tables } = usePermissionTables({
      dataSourceName: 'default',
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={p => {
          setPermissions(p);
          args.onPermissionsChange?.(p);
        }}
        table={{ schema: 'public', name: 'Stuff' }}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={permissions}
      />
    );
  },

  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    // Wait until Loading is gone
    await waitForElementToBeRemoved(() => canvas.queryByText('Loading'), {
      timeout: 5000,
    });

    // // Write a number in the input
    await userEvent.type(canvas.getByTestId('id._eq-value-input'), '1337');

    expect(args.onPermissionsChange).toHaveBeenCalledWith({
      id: {
        _eq: 12341337,
      },
    });
  },
};

export const OperatorDropdownHandling: StoryObj<typeof RowPermissionsInput> = {
  render: args => (
    <RowPermissionsInput
      onPermissionsChange={action('onPermissionsChange')}
      table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
      tables={tables}
      comparators={comparators}
      logicalModel={undefined}
      logicalModels={[]}
      permissions={{
        _not: { STATUS: { _eq: 'X-Hasura-User-Id' } },
      }}
    />
  ),

  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await userEvent.selectOptions(canvas.getByTestId('_not-operator'), '_or');

    await userEvent.selectOptions(
      canvas.getByTestId('_or-operator'),
      '_exists'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_exists-operator'),
      '_and'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_and.1-operator'),
      'Period'
    );

    await userEvent.selectOptions(canvas.getByTestId('_and-operator'), '_or');

    await userEvent.selectOptions(
      canvas.getByTestId('_or.1-operator'),
      'Period'
    );
  },
};
