import { ComponentStory, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { RowPermissionsInput } from './RowPermissionsInput';
import { within } from '@testing-library/dom';
import { userEvent, waitFor } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { tables } from './__tests__/fixtures/tables';
import { comparators } from './__tests__/fixtures/comparators';

export default {
  title: 'Features/Permissions/Form/Row Permissions Input',
  component: RowPermissionsInput,
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

SetRootLevelPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await userEvent.click(canvas.getByTestId('-select'));
  await userEvent.selectOptions(canvas.getByTestId('-select'), 'Subject');
};

SetExistsPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-select'), '_exists');

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._table-select'),
    'Label'
  );

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._where-select-empty'),
    'id'
  );

  await userEvent.type(
    canvas.getByTestId('_exists._where.id._eq-input'),
    '1337'
  );
};

SetMultilevelExistsPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-select'), '_exists');

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._table-select'),
    'Label'
  );

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._where-select-empty'),
    '_exists'
  );

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._where._exists._table-select'),
    'Label'
  );

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._where._exists._where-select-empty'),
    'id'
  );

  await userEvent.type(
    canvas.getByTestId('_exists._where._exists._where.id._eq-input'),
    '1337'
  );
};

SetAndPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-select'), '_and');

  await userEvent.selectOptions(
    canvas.getByTestId('_and.1-select-empty'),
    'Series_reference'
  );

  await userEvent.type(
    canvas.getByTestId('_and.1.Series_reference._eq-input'),
    '1337',
    { delay: 300 }
  );
};

SetMultilevelAndPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-select'), '_and');

  await userEvent.selectOptions(
    canvas.getByTestId('_and.1-select-empty'),
    'Series_reference'
  );

  await userEvent.type(
    canvas.getByTestId('_and.1.Series_reference._eq-input'),
    '1337',
    { delay: 300 }
  );

  await userEvent.selectOptions(
    canvas.getByTestId('_and.2-select-empty'),
    'STATUS'
  );
  await userEvent.type(canvas.getByTestId('_and.2.STATUS._eq-input'), '1338');
};

SetNotPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-select'), '_not');

  await userEvent.selectOptions(
    canvas.getByTestId('_not-select-empty'),
    'Period'
  );

  await userEvent.selectOptions(
    canvas.getByTestId('_not.Period._eq-select'),
    '_neq'
  );
};

SetOrPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-select'), '_or');

  await userEvent.selectOptions(
    canvas.getByTestId('_or.1-select-empty'),
    'Period'
  );

  await userEvent.type(canvas.getByTestId('_or.1.Period._eq-input'), '1337');
};

SetMultilevelOrPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await userEvent.selectOptions(canvas.getByTestId('-select'), '_or');

  await userEvent.selectOptions(
    canvas.getByTestId('_or.1-select-empty'),
    'Series_reference'
  );

  await userEvent.type(
    canvas.getByTestId('_or.1.Series_reference._eq-input'),
    '1337',
    { delay: 300 }
  );

  await userEvent.selectOptions(
    canvas.getByTestId('_or.2-select-empty'),
    'STATUS'
  );
  await userEvent.type(canvas.getByTestId('_or.2.STATUS._eq-input'), '1338', {
    delay: 300,
  });
};

SetDisabledExistsPermission.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  const existElement = canvas.getByTestId('_exists._where-input-no-value');
  expect(existElement).toHaveAttribute('disabled');

  await userEvent.selectOptions(
    canvas.getByTestId('_exists._table-select'),
    'Label'
  );

  expect(existElement).not.toHaveAttribute('disabled');
};
