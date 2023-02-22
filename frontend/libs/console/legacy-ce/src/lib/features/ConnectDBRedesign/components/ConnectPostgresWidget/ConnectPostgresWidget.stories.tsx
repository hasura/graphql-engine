import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ConnectPostgresWidget } from './ConnectPostgresWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';

export default {
  component: ConnectPostgresWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ConnectPostgresWidget>;

export const PostgresCreateConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget />
    </div>
  );
};

export const PostgresEditConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget dataSourceName="chinook" />
    </div>
  );
};

export const AlloyDBCreateConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget overrideDisplayName="AlloyDB" />
    </div>
  );
};

export const CitusCreateConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget
        overrideDisplayName="Citus"
        overrideDriver="citus"
      />
    </div>
  );
};

export const CitusEditConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget
        overrideDisplayName="Citus"
        overrideDriver="citus"
        dataSourceName="citus_test"
      />
    </div>
  );
};

export const CockroachCreateConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget
        overrideDisplayName="CockroachDB"
        overrideDriver="cockroach"
      />
    </div>
  );
};
