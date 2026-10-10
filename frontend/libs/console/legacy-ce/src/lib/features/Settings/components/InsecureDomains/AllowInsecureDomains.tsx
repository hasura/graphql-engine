import React, { useState } from 'react';
import { Flex, Heading, Link } from '@radix-ui/themes';
import { Button, CardedTable, Text } from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import AddDomain from './AddDomain';
import { getConfirmation } from '@hasura/shared/utils';
import { useMetadata, useDeleteInsecureDomain } from '@hasura/metadata/api';

const InsecureDomains: React.FC = () => {
  const [toggle, setToggle] = useState(false);
  const { data: insecureDomains } = useMetadata(
    (m) => m.metadata.network?.tls_allowlist,
  );
  const deleteInsecureDomain = useDeleteInsecureDomain();

  const handleDeleteDomain = (host: string, port?: string) => {
    const confirmMessage = `This will permanently delete the domain.`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      deleteInsecureDomain(host, port);
    }
  };

  return (
    <Analytics name="InsecureDomains" {...REDACT_EVERYTHING}>
      <Flex className="p-4" gap="4" direction="column">
        <Heading size="6"> Insecure TLS Allow List </Heading>
        <Text as="p">
          Allow your HTTPS integrations (Actions, Event Triggers, Cron
          Triggers,etc) to use self-signed certificates. For more information
          refer to{' '}
          <Link
            href="https://hasura.io/docs/latest/deployment/tls-allow-list/"
            target="_blank"
            rel="noopener noreferrer"
          >
            docs
          </Link>{' '}
          here.
        </Text>
        <CardedTable
          columns={['DOMAIN', 'MODIFY']}
          data={
            insecureDomains?.length
              ? insecureDomains.map((domain) => {
                  return [
                    domain.suffix
                      ? `${domain.host}:${domain.suffix}`
                      : `${domain.host}`,

                    <Button
                      key={`${domain.host}:${domain.suffix}`}
                      mode="destructive"
                      size="sm"
                      onClick={() =>
                        handleDeleteDomain(domain.host, domain.suffix)
                      }
                      data-test={`delete-domain-${domain.host}`}
                    >
                      Delete
                    </Button>,
                  ];
                })
              : [
                  [
                    <Text key="no-domains">
                      No domains added to insecure TLS allow list
                    </Text>,
                  ],
                ]
          }
        />
        {!toggle ? (
          <Button
            mode="default"
            data-test="add-insecure-domain"
            onClick={() => setToggle(true)}
          >
            Add Domain
          </Button>
        ) : (
          <AddDomain setToggle={() => setToggle(false)} />
        )}
      </Flex>
    </Analytics>
  );
};

export default InsecureDomains;
