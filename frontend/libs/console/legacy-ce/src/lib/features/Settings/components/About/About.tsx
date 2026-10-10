import React from 'react';
import { Analytics } from '@hasura/shared/analytics';
import { EELicenseInfo } from './EELicenseInfo';
import { LabelValue } from './LabelValue';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { Spinner } from '@hasura/shared/ui';
import { Flex, Heading } from '@radix-ui/themes';

export const About: React.FC = () => {
  useDocumentTitle('About | Hasura');
  const { serverVersion } = useAppContext();

  return (
    <Analytics name="About">
      <div className="p-6">
        <Flex direction="column" gap="4">
          <Heading size="5">About</Heading>
          <LabelValue
            label={'Current Server Version'}
            value={serverVersion || <Spinner />}
          />
          <LabelValue
            label={'Console asset version'}
            value={CONSOLE_ASSET_VERSION || 'NA'}
          />
          <EELicenseInfo />
        </Flex>
      </div>
    </Analytics>
  );
};

export default About;
