import { Button, Card, GraphqlCodeBlock, Text } from '@hasura/shared/ui';
import { FaPlayCircle } from 'react-icons/fa';
import { Analytics } from '@hasura/shared/analytics';
import { Flex, Strong } from '@radix-ui/themes';

export interface Props {
  schemaImage: string;
  onRunHandler: () => void;
  onSkipHandler: () => void;
  query: string;
}

export function QueryScreen(props: Props) {
  const { schemaImage, onRunHandler, onSkipHandler, query } = props;
  return (
    <Card className="overflow-auto mb-4">
      <Flex direction="column" align="center" justify="center">
        <div>
          <div>
            <img className="mb-4" src={schemaImage} alt="graphql-schema" />
          </div>
          <Flex justify="center" className="mb-4">
            <Text>
              We&apos;ve created a structure with two tables{' '}
              <Strong>customer</Strong> and <Strong>order</Strong> connected
              through a foreign key relationship.
            </Text>
          </Flex>
          <div className="w-full" data-testid="query-dialog-sample-query">
            <pre className="px-4 py-4">
              <Text as="p" weight="bold">
                SAMPLE GRAPHQL QUERY
              </Text>
              <GraphqlCodeBlock text={query} className="mt-2" />
            </pre>
          </div>
        </div>
      </Flex>

      <div className="w-full">
        <div className="w-full mb-2">
          <Card>
            <Flex align="center">
              <Flex align="center" className="w-3/4">
                <Text size="3">
                  <span className="mr-1" role="img" aria-label="rocket">
                    🚀
                  </span>{' '}
                  <Strong>You&apos;re ready to go!</Strong> Run your first
                  sample query to get started.
                </Text>
              </Flex>
              <Flex justify="end" className="w-1/4">
                <Analytics
                  name="query-screen-get-started-button"
                  passHtmlAttributesToChildren
                >
                  <Button
                    mode="primary"
                    onClick={onRunHandler}
                    rightIcon={FaPlayCircle}
                  >
                    Run a Sample Query
                  </Button>
                </Analytics>
              </Flex>
            </Flex>
          </Card>
        </div>
        <Flex justify="start" align="center" className="w-full mt-2">
          <Analytics name="onboarding-skip-button">
            <Button
              variant="ghost"
              size="1"
              color="gray"
              className="w-auto"
              onClick={() => {
                onSkipHandler();
              }}
            >
              Skip, continue to Console
            </Button>
          </Analytics>
        </Flex>
      </div>
    </Card>
  );
}
