import React, { useState } from 'react';
import { Flex, Strong } from '@radix-ui/themes';
import {
  Button,
  Card,
  IconTooltip,
  Input,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import { useAddInsecureDomain } from '@hasura/metadata/api';

type Props = { setToggle: () => void };

const AddDomain: React.FC<Props> = ({ setToggle }) => {
  const [domainName, setDomainName] = useState('');
  const addInsecureDomain = useAddInsecureDomain();

  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setDomainName(e.target.value);
  };

  const saveWithToggle = () => {
    const url = domainName.split(':');
    const host = url[0];
    const port = url[1];
    addInsecureDomain(host, port).then(() => {
      setToggle();
    });
  };

  return (
    <Card>
      <Flex gap="4" direction="column">
        <Text>
          <Strong>Add Domain to Insecure TLS Allow List</Strong>
        </Text>
        <Flex align="center" gap="2">
          <Text>
            <Strong>Domain Name</Strong>
          </Text>
          <IconTooltip message="The domain to be added to the allow list. The format is hostname:port, where port is optional." />
          <LearnMoreLink href="https://hasura.io/docs/latest/deployment/tls-allow-list/" />
        </Flex>
        <Input
          type="text"
          prependLabel="https://"
          className={`rounded-bl-none rounded-tl-none`}
          placeholder="mydomain.com:8080"
          data-test="domain-name"
          value={domainName}
          onChange={handleNameChange}
        />
        <Flex gap="2" align="center">
          <Button
            mode="default"
            data-test="cancel-domain"
            onClick={() => {
              setToggle();
            }}
          >
            Cancel
          </Button>
          <Button
            type="submit"
            mode="primary"
            data-test="add-tls-allow-list"
            onClick={saveWithToggle}
          >
            Add to Allow List
          </Button>
        </Flex>
      </Flex>
    </Card>
  );
};

export default AddDomain;
