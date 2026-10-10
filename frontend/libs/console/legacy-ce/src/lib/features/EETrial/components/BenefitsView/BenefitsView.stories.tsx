import { StoryObj, Meta } from '@storybook/react-webpack5';
import { BenefitsView } from './BenefitsView';
import { Dialog } from '@hasura/shared/ui';

export default {
  title: 'features/EETrial/BenefitsView 🧬️',
  parameters: {
    Benefits: {
      source: { type: 'code' },
    },
  },
  component: BenefitsView,
} as Meta<typeof BenefitsView>;

export const NoEnterpriseLicense: StoryObj<typeof BenefitsView> = {
  render: (args) => (
    <Dialog size="md" onClose={() => {}}>
      <BenefitsView
        licenseInfo={{
          status: 'none',
          type: 'trial',
          grace_at: new Date(),
          expiry_at: new Date(),
        }}
      />
    </Dialog>
  ),
};

export const ActiveEnterpriceLicense: StoryObj<typeof BenefitsView> = {
  render: (args) => (
    <Dialog size="md" onClose={() => {}}>
      <BenefitsView
        licenseInfo={{
          status: 'active',
          type: 'trial',
          grace_at: new Date(),
          expiry_at: new Date(new Date().getTime() + 86405000),
        }}
      />
    </Dialog>
  ),
};

export const ExpiredEnterpriseLicenseWithGrace: StoryObj<typeof BenefitsView> =
  {
    render: (args) => (
      <Dialog size="md" onClose={() => {}}>
        <BenefitsView
          licenseInfo={{
            status: 'expired',
            type: 'trial',
            grace_at: new Date(new Date().getTime() + 100000000),
            expiry_at: new Date(new Date().getTime() - 100000000),
          }}
        />
      </Dialog>
    ),
  };

export const ExpiredEnterpriseLicenseWithoutGrace: StoryObj<
  typeof BenefitsView
> = {
  render: (args) => (
    <Dialog size="md" onClose={() => {}}>
      <BenefitsView
        licenseInfo={{
          status: 'expired',
          type: 'trial',
          expiry_at: new Date(new Date().getTime() - 100000000),
        }}
      />
    </Dialog>
  ),
};

export const ExpiredEnterpriseLicenseAfterGrace: StoryObj<typeof BenefitsView> =
  {
    render: (args) => (
      <Dialog size="md" onClose={() => {}}>
        <BenefitsView
          licenseInfo={{
            status: 'expired',
            type: 'trial',
            expiry_at: new Date(new Date().getTime() - 200000000),
            grace_at: new Date(new Date().getTime() - 100000000),
          }}
        />
      </Dialog>
    ),
  };

export const DeactivatedEnterpriseLicenseAfterGrace: StoryObj<
  typeof BenefitsView
> = {
  render: (args) => (
    <Dialog size="md" onClose={() => {}}>
      <BenefitsView
        licenseInfo={{
          status: 'deactivated',
          type: 'trial',
          expiry_at: new Date(new Date().getTime() - 200000000),
          grace_at: new Date(new Date().getTime() - 100000000),
        }}
      />
    </Dialog>
  ),
};
