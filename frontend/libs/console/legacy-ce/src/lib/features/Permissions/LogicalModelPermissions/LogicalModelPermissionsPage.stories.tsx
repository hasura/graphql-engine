import { Meta, StoryObj } from '@storybook/react';
import { LogicalModelPermissionsPage } from './LogicalModelPermissionsPage';
import { handlers, deleteHandlers } from './mocks';

import { RouteWrapper } from '../../Data/LogicalModels/components/RouteWrapper';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ConsoleTypeDecorator } from '../../../storybook/decorators';

const name = 'LogicalModel';
const source = 'Postgres';

export default {
  component: params => {
    return (
      <RouteWrapper
        route={
          '/data/native-queries/logical-models/{{source}}/{{name}}/permissions'
        }
        itemSourceName={source}
        itemName={name}
      >
        <LogicalModelPermissionsPage {...params} />
      </RouteWrapper>
    );
  },
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro' }),
  ],
} as Meta;

type Story = StoryObj<typeof LogicalModelPermissionsPage>;

export const SavePermission: Story = {
  args: {
    name,
    source,
  },
  // Play function deleted because it was failing in chromatic
  // It works locally and in storybook, but not in chromatic
  // TODO: Figure out the issue
  // play: async ({ canvasElement }) => {
  //   const canvas = within(canvasElement);
  //   // Wait for both tabs and loading to be present because otherwise story fails in chromatic
  //   expect(
  //     await canvas.findByTestId(
  //       'logical-model-permissions-tab',
  //       {},
  //       {
  //         timeout: 10000,
  //       }
  //     )
  //   ).toBeInTheDocument();
  //   expect(
  //     await canvas.findByTestId('loading-logical-model-permissions')
  //   ).toBeInTheDocument();
  //   await canvas.findByTestId('permissions-table');
  //   await userEvent.click(
  //     await canvas.findByTestId('editor-select-permissions-cell')
  //   );
  //   await userEvent.click(await canvas.findByTestId('save-permissions-button'));
  //   await expect(
  //     await canvas.findByText('Permissions saved successfully!')
  //   ).toBeInTheDocument();
  // },
  parameters: {
    msw: handlers(),
  },
};

export const DeletePermission: Story = {
  args: {
    name,
    source,
  },
  // Play function deleted because it was failing in chromatic
  // It works locally and in storybook, but not in chromatic
  // TODO: Figure out the issue
  // play: async ({ canvasElement }) => {
  //   const canvas = within(canvasElement);
  //   // Wait for both tabs and loading to be present because otherwise story fails in chromatic
  //   expect(
  //     await canvas.findByTestId(
  //       'logical-model-permissions-tab',
  //       {},
  //       {
  //         timeout: 10000,
  //       }
  //     )
  //   );
  //   expect(
  //     await canvas.findByTestId('loading-logical-model-permissions')
  //   ).toBeInTheDocument();
  //   await canvas.findByTestId('permissions-table');
  //   await userEvent.click(
  //     await canvas.findByTestId('editor-select-permissions-cell')
  //   );
  //   await userEvent.click(
  //     await canvas.findByTestId('delete-permissions-button')
  //   );
  //   await expect(
  //     await canvas.findByText('Permissions successfully deleted!')
  //   ).toBeInTheDocument();
  // },
  parameters: {
    msw: deleteHandlers(),
  },
};
