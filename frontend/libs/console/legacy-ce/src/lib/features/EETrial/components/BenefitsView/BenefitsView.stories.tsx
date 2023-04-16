import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';

import { BenefitsView } from './BenefitsView';
import { Dialog } from '../../../../new-components/Dialog';

export default {
  title: 'features/EETrial/ BenefitsView 🧬️',
  parameters: {
    Benefits: {
      source: { type: 'code' },
    },
  },
  component: BenefitsView,
} as ComponentMeta<typeof BenefitsView>;

export const NoEnterpriseLicense: ComponentStory<
  typeof BenefitsView
> = args => (
  <Dialog size="md" onClose={() => {}} hasBackdrop>
    <BenefitsView
      licenseInfo={{
        state: 'none',
        type: 'trial',
        grace_at: new Date(),
        expiry_at: new Date(),
      }}
    />
  </Dialog>
);

export const ActiveEnterpriceLicense: ComponentStory<
  typeof BenefitsView
> = args => (
  <Dialog size="md" onClose={() => {}} hasBackdrop>
    <BenefitsView
      licenseInfo={{
        state: 'active',
        type: 'trial',
        grace_at: new Date(),
        expiry_at: new Date(new Date().getTime() + 86405000),
      }}
    />
  </Dialog>
);

export const ExpiredEnterpriseLicenseWithGrace: ComponentStory<
  typeof BenefitsView
> = args => (
  <Dialog size="md" onClose={() => {}} hasBackdrop>
    <BenefitsView
      licenseInfo={{
        state: 'expired',
        type: 'trial',
        grace_at: new Date(new Date().getTime() + 100000000),
        expiry_at: new Date(new Date().getTime() - 100000000),
      }}
    />
  </Dialog>
);

export const ExpiredEnterpriseLicenseWithoutGrace: ComponentStory<
  typeof BenefitsView
> = args => (
  <Dialog size="md" onClose={() => {}} hasBackdrop>
    <BenefitsView
      licenseInfo={{
        state: 'expired',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 100000000),
      }}
    />
  </Dialog>
);

export const ExpiredEnterpriseLicenseAfterGrace: ComponentStory<
  typeof BenefitsView
> = args => (
  <Dialog size="md" onClose={() => {}} hasBackdrop>
    <BenefitsView
      licenseInfo={{
        state: 'expired',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 200000000),
        grace_at: new Date(new Date().getTime() - 100000000),
      }}
    />
  </Dialog>
);

export const DeactivatedEnterpriseLicenseAfterGrace: ComponentStory<
  typeof BenefitsView
> = args => (
  <Dialog size="md" onClose={() => {}} hasBackdrop>
    <BenefitsView
      licenseInfo={{
        state: 'deactivated',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 200000000),
        grace_at: new Date(new Date().getTime() - 100000000),
      }}
    />
  </Dialog>
);
