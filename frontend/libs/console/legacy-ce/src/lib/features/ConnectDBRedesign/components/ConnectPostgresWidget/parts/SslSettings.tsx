import {
  Collapsible,
  InputField,
  SelectField,
  IconTooltip,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const SslSettings = ({ name }: { name: string }) => {
  return (
    <Collapsible
      triggerChildren={
        <Flex align="center" gap="2" className="cursor-pointer">
          <Text as="span" weight="bold">
            SSL Certificates Settings
          </Text>
          <IconTooltip message="Certificates will be loaded from environment variables" />
          <LearnMoreLink href="https://hasura.io/docs/2.0/databases/postgres/gcp/#step-72-add-env-vars" />
        </Flex>
      }
      disableContentStyles
    >
      <div className="mt-2">
        <div className="mb-4">
          <SelectField
            options={[
              {
                value: 'disable',
                label: 'disable',
              },
              {
                value: 'verify-ca',
                label: 'verify-ca',
              },
              {
                value: 'verify-full',
                label: 'verify-full',
              },
            ]}
            name={`${name}.sslMode`}
            label="SSL Mode"
            placeholder="-- Select --"
            tooltip="SSL certificate verification mode"
          />
        </div>
        <InputField
          name={`${name}.sslRootCert`}
          label="SSL Root Certificate"
          tooltip="Environment variable that stores trusted certificate authorities"
          fieldProps={{ placeholder: 'SSL_ROOT_CERT' }}
        />
        <InputField
          name={`${name}.sslCert`}
          label="SSL Certificate"
          tooltip="Environment variable that stores the client certificate (Optional)"
          fieldProps={{ placeholder: 'SSL_CERT' }}
        />
        <InputField
          name={`${name}.sslKey`}
          label="SSL Key"
          tooltip="Environment variable that stores the client private key (Optional)"
          fieldProps={{ placeholder: 'SSL_KEY' }}
        />
        <InputField
          name={`${name}.sslPassword`}
          label="SSL Password"
          tooltip="Environment variable that stores the password if the client private key is encrypted (Optional)"
          fieldProps={{ placeholder: 'SSL_PASSWORD' }}
        />
      </div>
    </Collapsible>
  );
};
