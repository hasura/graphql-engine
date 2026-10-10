import { FaExternalLinkAlt, FaFileImport, FaRegEdit } from 'react-icons/fa';
import { Dialog } from '../../Dialog';
import { Badge } from '../../Badge';
import { Button } from '../../Button';
import { Em, Flex, Heading, Link } from '@radix-ui/themes';
import { Text } from '../../typography';
import { Card } from '../../Card';
import { Separator } from '../../Separator';

type props = {
  handleActionForm: () => void;
  handleOpenApiForm: () => void;
};

export const ActionInitialModal = ({
  handleActionForm,
  handleOpenApiForm,
}: props) => {
  return (
    <Dialog onClose={() => {}}>
      <>
        <div className="px-6 py-8">
          <Heading size="4">Let&apos;s create an Action from</Heading>
          <Text>Two options available to create an Action</Text>
          <Flex gap="4" className="pt-4">
            <Card className="w-1/2">
              <Flex className="mb-2" align="center" justify="between">
                <Text weight="bold" color="gray">
                  Action Form
                </Text>
                <Badge color="gray">Default</Badge>
              </Flex>
              <div className="mb-4">
                <Text color="gray" size="1">
                  Create your Action via form input manually.
                </Text>
              </div>
              <Button leftIcon={FaRegEdit} size="sm" onClick={handleActionForm}>
                Fill Action Form
              </Button>
            </Card>
            <Card className="w-1/2">
              <Flex className="mb-2" align="center" justify="between">
                <Text weight="bold" color="gray">
                  OpenAPI Spec
                </Text>
                <Badge color="purple">New</Badge>
              </Flex>
              <div className="mb-4">
                <Text color="gray" size="1">
                  Generate Actions from OpenAPI spec (OAS).
                </Text>
              </div>
              <Button
                leftIcon={FaFileImport}
                type="submit"
                mode="primary"
                size="sm"
                onClick={handleOpenApiForm}
              >
                Import from OAS
              </Button>
            </Card>
          </Flex>
        </div>
        <Separator className="mb-2" size="4" />
        <Flex asChild gap="2" align="center">
          <Link
            href="https://hasura.io/docs/latest/actions/create/"
            target="_blank"
            rel="noopener noreferrer"
            size="1"
          >
            <Em>Learn more about creating Actions in Hasura</Em>
            <FaExternalLinkAlt className="pl-1" />
          </Link>
        </Flex>
      </>
    </Dialog>
  );
};
