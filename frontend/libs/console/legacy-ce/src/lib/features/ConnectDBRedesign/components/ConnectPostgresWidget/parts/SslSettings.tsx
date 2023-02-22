import { InputField, Select } from '../../../../../new-components/Form';

export const SslSettings = ({ name }: { name: string }) => {
  return (
    <>
      <Select
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
        tooltip="SSL certificate verification mode"
      />
      <InputField
        name={`${name}.sslRootCert`}
        label="SSL Root Certificate"
        placeholder="SSL_ROOT_CERT"
        tooltip="Environment variable that stores trusted certificate authorities"
      />
      <InputField
        name={`${name}.sslCert`}
        label="SSL Certificate"
        placeholder="SSL_CERT"
        tooltip="Environment variable that stores the client certificate (Optional)"
      />
      <InputField
        name={`${name}.sslKey`}
        label="SSL Key"
        placeholder="SSL_KEY"
        tooltip="Environment variable that stores the client private key (Optional)"
      />
      <InputField
        name={`${name}.sslPassword`}
        label="SSL Password"
        placeholder="SSL_PASSWORD"
        tooltip="Environment variable that stores the password if the client private key is encrypted (Optional)"
      />
    </>
  );
};
