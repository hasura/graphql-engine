import { ComponentStory, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { RowPermissionsInput } from './RowPermissionsInput';
import { within } from '@testing-library/dom';
import {
  fireEvent,
  userEvent,
  waitFor,
  waitForElementToBeRemoved,
} from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { tables } from './__tests__/fixtures/tables';
import { comparators } from './__tests__/fixtures/comparators';
import { usePermissionTables } from '../hooks/usePermissionTables';
import { usePermissionComparators } from '../hooks/usePermissionComparators';
import { handlers } from './__tests__/fixtures/jsonb/handlers';
import { ReactQueryDecorator } from '../../../../../../storybook/decorators/react-query';
import { isEmpty } from 'lodash';
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

export const SetRootLevelPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const SetExistsPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const SetMultilevelExistsPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const SetAndPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const SetMultilevelAndPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const SetNotPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const SetOrPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const SetMultilevelOrPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const Empty: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{}}
  />
);

export const Exists: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{
      _exists: {
        _table: {},
        _where: {},
      },
    }}
  />
);

export const SetDisabledExistsPermission: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{
      _exists: {
        _table: {},
        _where: {},
      },
    }}
  />
);

export const ExistsWhere: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
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
);

export const EmptyExists: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{
      _exists: {
        _table: {},
        _where: {},
      },
    }}
  />
);

export const And: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{
      _and: [
        { STATUS: { _eq: 'X-Hasura-User-Id' } },
        { Period: { _eq: 'Period' } },
      ],
    }}
  />
);

export const EmptyAnd: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{
      _and: [{}],
    }}
  />
);

export const Not: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{
      _not: { STATUS: { _eq: 'X-Hasura-User-Id' } },
    }}
  />
);

export const EmptyNot: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{
      _not: {},
    }}
  />
);

export const Relationships: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={['Album']}
    tables={tables}
    comparators={comparators}
    permissions={{ Author: { name: { _eq: '' } } }}
  />
);

export const RelationshipsColumns: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={['Album']}
    tables={tables}
    comparators={comparators}
    permissions={{ Label: { id: { _eq: '' } } }}
  />
);

export const ColumnTypes: ComponentStory<typeof RowPermissionsInput> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={{ dataset: 'bigquery_sample', name: 'sample_table' }}
    tables={tables}
    comparators={comparators}
    permissions={{ Series_reference: { _eq: '' } }}
  />
);

export const BooleanArrayType: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={['Album']}
    tables={tables}
    comparators={comparators}
    permissions={{ Author: { _ceq: ['name'] } }}
  />
);

BooleanArrayType.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByTestId('Author-operator')).toBeInTheDocument();
  expect(canvas.getByTestId('Author._ceq-comparator')).toBeInTheDocument();
  expect(
    canvas.getByTestId('Author._ceq-column-comparator-entry')
  ).toBeInTheDocument();
};

export const BooleanArrayTypeRoot: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={['Album']}
    tables={tables}
    comparators={comparators}
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
);

BooleanArrayTypeRoot.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  expect(canvas.getByTestId('Author._ceq-column-comparator-entry')).toHaveValue(
    '$'
  );

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
};

export const NumericValue: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={['Album']}
    tables={tables}
    comparators={comparators}
    permissions={{ id: { _eq: '' } }}
  />
);

NumericValue.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(
    canvas.getByTestId('id._eq-value-input-x-hasura-user-id')
  ).toBeInTheDocument();
};

export const NumericIntValue: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={['Album']}
    tables={tables}
    comparators={comparators}
    permissions={{ id: { _eq: 0 } }}
  />
);

export const NumericFloatValue: ComponentStory<
  typeof RowPermissionsInput
> = args => (
  <RowPermissionsInput
    onPermissionsChange={action('onPermissionsChange')}
    table={['Album']}
    tables={tables}
    comparators={comparators}
    permissions={{ id: { _eq: 0.9 } }}
  />
);

SetRootLevelPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByTestId('-operator'));
  await userEvent.selectOptions(canvas.getByTestId('-operator'), 'Subject');
};

SetExistsPermission.play = async ({ canvasElement }) => {
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
};

SetMultilevelExistsPermission.play = async ({ canvasElement }) => {
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
};

SetAndPermission.play = async ({ canvasElement }) => {
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
};

SetMultilevelAndPermission.play = async ({ canvasElement }) => {
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
};

SetNotPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-operator'), '_not');

  await userEvent.selectOptions(canvas.getByTestId('_not-operator'), 'Period');

  await userEvent.selectOptions(
    canvas.getByTestId('_not.Period._eq-comparator'),
    '_neq'
  );
};

SetOrPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-operator'), '_or');

  await userEvent.selectOptions(canvas.getByTestId('_or.1-operator'), 'Period');

  await userEvent.type(
    canvas.getByTestId('_or.1.Period._eq-value-input'),
    '1337'
  );

  expect(canvas.getByTestId('_or.1.Period._eq-value-input')).toHaveValue(
    '1337'
  );
};

SetMultilevelOrPermission.play = async ({ canvasElement }) => {
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

  await userEvent.selectOptions(canvas.getByTestId('_or.2-operator'), 'STATUS');
  await userEvent.type(
    canvas.getByTestId('_or.2.STATUS._eq-value-input'),
    '1338',
    {
      delay: 300,
    }
  );
};

SetDisabledExistsPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  const existElement = canvas.getByTestId('_exists._where-value-input');
  expect(existElement).toHaveAttribute('disabled');

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._table-value-input'),
    'Label'
  );

  expect(existElement).not.toHaveAttribute('disabled');
};

export const JsonbColumns: ComponentStory<
  typeof RowPermissionsInput
> = args => {
  const tables = usePermissionTables({
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
      permissions={{ jason: { _contained_in: { a: 'b' } } }}
    />
  );
};

JsonbColumns.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Wait until Loading is gone
  await waitForElementToBeRemoved(() => canvas.getByText('Loading'), {
    timeout: 50000,
  });
  // Expect jason._contained_in-comparator to be in the document
  expect(
    canvas.getByTestId('jason._contained_in-comparator')
  ).toBeInTheDocument();
  // Expect jason._contained_in-value-input to have value "{"a": "b"}"
  expect(canvas.getByTestId('jason._contained_in-value-input')).toHaveValue(
    '{"a":"b"}'
  );
};

export const JsonbColumnsHasKeys: ComponentStory<
  typeof RowPermissionsInput
> = args => {
  const tables = usePermissionTables({
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
      permissions={{ jason: { _has_keys_all: [''] } }}
    />
  );
};

export const StringColumns: ComponentStory<
  typeof RowPermissionsInput
> = args => {
  const [permissions, setPermissions] = useState<Permissions>({
    name: { _eq: '' },
  });
  const tables = usePermissionTables({
    dataSourceName: 'default',
  });

  const comparators = usePermissionComparators();

  if (!tables || isEmpty(comparators)) return <>Loading</>;
  return (
    <>
      <RowPermissionsInput
        onPermissionsChange={p => {
          setPermissions(p);
        }}
        table={{ schema: 'public', name: 'Stuff' }}
        tables={tables}
        comparators={comparators}
        permissions={{ name: { _eq: '' } }}
      />
      {/* State debugger. Used to test the current permissions value */}
      <div className="invisible" data-testid="permissions-state">
        {JSON.stringify(permissions)}
      </div>
    </>
  );
};

// Play function testing that string column with numbers inside are represented as text on the UI
// Prevents regression where they were treated as numbers
StringColumns.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Wait until Loading is gone
  await waitForElementToBeRemoved(() => canvas.getByText('Loading'), {
    timeout: 50000,
  });

  // // Write a number in the input
  await userEvent.type(canvas.getByTestId('name._eq-value-input'), '1337');

  // Expect the input to have the number as text
  await waitFor(async () => {
    expect(await canvas.findByTestId('permissions-state')).toHaveTextContent(
      '{"name":{"_eq":"1337"}}'
    );
  });
};

export const NumberColumns: ComponentStory<
  typeof RowPermissionsInput
> = args => {
  const [permissions, setPermissions] = useState<Permissions>({
    id: { _eq: '' },
  });
  const tables = usePermissionTables({
    dataSourceName: 'default',
  });

  const comparators = usePermissionComparators();

  if (!tables || isEmpty(comparators)) return <>Loading</>;
  return (
    <>
      <RowPermissionsInput
        onPermissionsChange={p => {
          setPermissions(p);
        }}
        table={{ schema: 'public', name: 'Stuff' }}
        tables={tables}
        comparators={comparators}
        permissions={{ id: { _eq: '1234' } }}
      />
      {/* State debugger. Used to test the current permissions value */}
      <div className="invisible" data-testid="permissions-state">
        {JSON.stringify(permissions)}
      </div>
    </>
  );
};

NumberColumns.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Wait until Loading is gone
  await waitForElementToBeRemoved(() => canvas.getByText('Loading'), {
    timeout: 50000,
  });

  // // Write a number in the input
  await userEvent.type(canvas.getByTestId('id._eq-value-input'), '1337');

  // Expect the input to have the number as text
  // Note it's not a number but a string
  await waitFor(async () => {
    expect(await canvas.findByTestId('permissions-state')).toHaveTextContent(
      '{"id":{"_eq":"12341337"}}'
    );
  });
};
