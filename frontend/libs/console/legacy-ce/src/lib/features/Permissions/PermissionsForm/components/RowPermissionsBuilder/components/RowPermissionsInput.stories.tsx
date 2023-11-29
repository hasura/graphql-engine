import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { RowPermissionsInput } from './RowPermissionsInput';
import { waitFor, within } from '@storybook/testing-library';
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
import { handlers as jsonbHandlers } from './__tests__/fixtures/jsonb/handlers';
import { handlers as manyDbsHandlers } from './__tests__/fixtures/many-dbs/handlers';
import { handlers as mongoHandlers } from './__tests__/fixtures/mongo/handlers';
import { ReactQueryDecorator } from '../../../../../../storybook/decorators/react-query';
import isEmpty from 'lodash/isEmpty';
import { useState } from 'react';
import { Permissions } from './types';

export default {
  component: RowPermissionsInput,
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
    await userEvent.click(canvas.getByTestId('root-operator'));
    await userEvent.selectOptions(
      canvas.getByTestId('root-operator'),
      'Subject'
    );
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

    await userEvent.selectOptions(
      canvas.getByTestId('root-operator'),
      '_exists'
    );

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

    await userEvent.selectOptions(
      canvas.getByTestId('root-operator'),
      '_exists'
    );

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

    await userEvent.selectOptions(canvas.getByTestId('root-operator'), '_and');

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

      await userEvent.selectOptions(
        canvas.getByTestId('root-operator'),
        '_and'
      );

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

    await userEvent.selectOptions(canvas.getByTestId('root-operator'), '_not');

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

    await userEvent.selectOptions(canvas.getByTestId('root-operator'), '_or');

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

    await userEvent.selectOptions(canvas.getByTestId('root-operator'), '_or');

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
    expect(canvas.getByTestId('Author-operator-root')).toBeInTheDocument();
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
      tablesToLoad: [
        {
          source: 'default',
          table: {
            name: 'Stuff',
            schema: 'public',
          },
        },
      ],
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

  parameters: {
    msw: jsonbHandlers(),
  },
};

export const JsonbColumnsHasKeys: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const { tables } = usePermissionTables({
      dataSourceName: 'default',
      tablesToLoad: [
        {
          source: 'default',
          table: {
            name: 'Stuff',
            schema: 'public',
          },
        },
      ],
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

  parameters: {
    msw: jsonbHandlers(),
  },
};

export const StringColumns: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const [permissions, setPermissions] = useState<Permissions>({
      name: { _eq: '' },
    });
    const { tables } = usePermissionTables({
      dataSourceName: 'default',
      tablesToLoad: [
        {
          source: 'default',
          table: {
            name: 'Stuff',
            schema: 'public',
          },
        },
      ],
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
        _eq: '1337',
      },
    });
  },

  parameters: {
    msw: jsonbHandlers(),
  },
};

export const NumberColumns: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const [permissions, setPermissions] = useState<Permissions>({
      id: { _eq: '1234' },
    });
    const { tables } = usePermissionTables({
      dataSourceName: 'default',
      tablesToLoad: [
        {
          source: 'default',
          table: {
            name: 'Stuff',
            schema: 'public',
          },
        },
      ],
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

    // Wait until it loads
    // This happens when id-operator-root selector has value id
    await waitFor(() => {
      expect(canvas.getByTestId('id-operator-root')).toHaveValue('id');
    });

    // Write a number in the input
    await userEvent.type(canvas.getByTestId('id._eq-value-input'), '1337');

    await waitFor(async () => {
      expect(args.onPermissionsChange).toHaveBeenCalledWith({
        id: {
          _eq: 12341337,
        },
      });
    });

    await waitFor(async () => {
      await userEvent.click(canvas.getByText('[x-hasura-user-id]'));
      expect(args.onPermissionsChange).toHaveBeenCalledWith({
        id: {
          _eq: 'X-Hasura-User-Id',
        },
      });
    });
  },

  parameters: {
    msw: jsonbHandlers(),
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

    await userEvent.selectOptions(
      canvas.getByTestId('_not-operator-root'),
      '_or'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_or-operator-root'),
      '_exists'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_exists-operator-root'),
      '_and'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_and.1-operator'),
      'Period'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_and-operator-root'),
      '_or'
    );

    await userEvent.selectOptions(
      canvas.getByTestId('_or.1-operator'),
      'Period'
    );
  },
};

export const ReplaceArrayWithColumn: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const [permissions, setPermissions] = useState<Permissions>({ _and: [{}] });
    return (
      <RowPermissionsInput
        onPermissionsChange={p => {
          setPermissions(p);
        }}
        table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={permissions}
      />
    );
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Should be able to select the _and dropdown and change it to be a column
    await userEvent.selectOptions(
      canvas.getByTestId('_and-operator-root'),
      'Series_reference'
    );
    expect(
      canvas.getByTestId('Series_reference-operator-root')
    ).toBeInTheDocument();
  },
};

// The difference between this and the previous story is that this one does not have a value in the array
// There was a bug where this case did not work, so adding a test for it
export const ReplaceEmptyArrayWithColumn: StoryObj<typeof RowPermissionsInput> =
  {
    render: args => {
      const [permissions, setPermissions] = useState<Permissions>({ _and: [] });
      return (
        <RowPermissionsInput
          onPermissionsChange={p => {
            setPermissions(p);
          }}
          table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
          tables={tables}
          comparators={comparators}
          logicalModel={undefined}
          logicalModels={[]}
          permissions={permissions}
        />
      );
    },
    play: async ({ canvasElement }) => {
      const canvas = within(canvasElement);
      // Should be able to select the _and dropdown and change it to be a column
      await userEvent.selectOptions(
        canvas.getByTestId('_and-operator-root'),
        'Series_reference'
      );
      expect(
        canvas.getByTestId('Series_reference-operator-root')
      ).toBeInTheDocument();
    },
  };

export const RemoteRelationships: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const [permissions, setPermissions] = useState<Permissions>({});
    const { tables } = usePermissionTables({
      dataSourceName: 'OhMy',
      tablesToLoad: [
        {
          source: 'Chinook',
          table: ['Chinook', 'Artist'],
        },
      ],
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={p => {
          setPermissions(p);
          args.onPermissionsChange?.(p);
        }}
        table={['Chinook', 'Artist']}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={permissions}
      />
    );
  },

  parameters: {
    msw: manyDbsHandlers(),
  },

  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    // Wait until Loading is gone
    await waitForElementToBeRemoved(() => canvas.queryByText('Loading'), {
      timeout: 5000,
    });

    // Open dropdown
    await userEvent.click(canvas.getByTestId('root-operator'));
    // Should not display remote relationships
    expect(canvas.queryByText('Album_Artist')).not.toBeInTheDocument();
  },
};

export const NestedObjects: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const { tables } = usePermissionTables({
      dataSourceName: 'M',
      tablesToLoad: [
        {
          source: 'M',
          table: ['students'],
        },
      ],
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={action('onPermissionsChange')}
        table={['students']}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={{ address: { city: { _eq: 'Moon' } } }}
      />
    );
  },

  parameters: {
    msw: mongoHandlers(),
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Should display city
    expect(
      await canvas.findByTestId('address.city-operator-root')
    ).toBeInTheDocument();
  },
};

export const NestedObjectsInitiallyEmpty: StoryObj<typeof RowPermissionsInput> =
  {
    render: args => {
      const { tables } = usePermissionTables({
        dataSourceName: 'M',
        tablesToLoad: [
          {
            source: 'M',
            table: ['students'],
          },
        ],
      });

      const comparators = usePermissionComparators();

      if (!tables || isEmpty(comparators)) return <>Loading</>;
      return (
        <RowPermissionsInput
          onPermissionsChange={action('onPermissionsChange')}
          table={['students']}
          tables={tables}
          comparators={comparators}
          logicalModel={undefined}
          logicalModels={[]}
          permissions={{}}
        />
      );
    },

    parameters: {
      msw: mongoHandlers(),
    },

    play: async ({ canvasElement }) => {
      const canvas = within(canvasElement);

      await waitFor(
        async () => {
          await canvas.findByTestId('RootInputReady');
        },
        { timeout: 1000 }
      );

      await canvas.findAllByRole('option', {
        name: 'address',
      });
      // Open root dropdown
      await userEvent.selectOptions(
        await canvas.findByTestId('root-operator'),
        'address'
      );
      // Open address dropdown
      await userEvent.selectOptions(
        await canvas.findByTestId('address-operator'),
        await canvas.findAllByRole('option', {
          name: 'city',
        })
      );
      // Should display city
      expect(
        await canvas.findByTestId('address.city-operator-root')
      ).toBeInTheDocument();
    },
  };

export const NestedObjectsAnd: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const { tables } = usePermissionTables({
      dataSourceName: 'M',
      tablesToLoad: [
        {
          source: 'M',
          table: ['students'],
        },
      ],
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={action('onPermissionsChange')}
        table={['students']}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={{ _and: [{ address: { city: { _eq: 'Moon' } } }] }}
      />
    );
  },

  parameters: {
    msw: mongoHandlers(),
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Should display city
    expect(
      await canvas.findByTestId('_and.0.address.city-operator-root')
    ).toBeInTheDocument();
  },
};

export const NestedObjectsOr: StoryObj<typeof RowPermissionsInput> = {
  render: args => {
    const { tables } = usePermissionTables({
      dataSourceName: 'M',
      tablesToLoad: [
        {
          source: 'M',
          table: ['students'],
        },
      ],
    });

    const comparators = usePermissionComparators();

    if (!tables || isEmpty(comparators)) return <>Loading</>;
    return (
      <RowPermissionsInput
        onPermissionsChange={action('onPermissionsChange')}
        table={['students']}
        tables={tables}
        comparators={comparators}
        logicalModel={undefined}
        logicalModels={[]}
        permissions={{ _or: [{}, { address: { city: { _eq: 'Moon' } } }] }}
      />
    );
  },

  parameters: {
    msw: mongoHandlers(),
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Should display city
    expect(
      await canvas.findByTestId('_or.1.address.city-operator-root')
    ).toBeInTheDocument();
  },
};
