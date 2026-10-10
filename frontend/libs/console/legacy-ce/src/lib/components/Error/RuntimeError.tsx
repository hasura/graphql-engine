import { useDocumentTitle } from '@hasura/shared/hooks';
import { Flex, Heading } from '@radix-ui/themes';
import {
  HasuraIcon,
  JavascriptCodeBlock,
  Link,
  RelativeLink,
  Text,
} from '@hasura/shared/ui';

const RuntimeError = ({ resetCallback, error }) => {
  useDocumentTitle('Error | Hasura');
  return (
    <Flex align="center" justify="center" className="h-screen w-screen">
      <Flex justify="center" align="center" gap="2">
        <div className="w-8/12">
          <Heading size="9">Error</Heading>
          <div>
            <Text>Something went wrong. Head back</Text>{' '}
            <RelativeLink size="2" to="/" onClick={resetCallback}>
              Home
            </RelativeLink>
            .
          </div>
          <br />
          <div>
            <JavascriptCodeBlock text={error.stack} scrollable />
          </div>
          <br />
          <div>
            <Text>You can report this issue on our</Text>{' '}
            <Link href="https://github.com/hasura/graphql-engine/issues">
              GitHub
            </Link>
          </div>
        </div>
        <div className="w-1/6 pl-16">
          <Text color="green">
            <HasuraIcon />
          </Text>
        </div>
      </Flex>
    </Flex>
  );
};

export default RuntimeError;
