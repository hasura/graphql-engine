import * as React from 'react';
import { Button, Card, Text } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { Flex } from '@radix-ui/themes';

type Props = {
  redirect: VoidFunction;
  timeSeconds: number;
};
export const RedirectCountDown: React.FC<Props> = (props) => {
  const { redirect, timeSeconds } = props;

  const [count, setCount] = React.useState(timeSeconds);
  const [loading, setLoading] = React.useState(false);

  const initiateRedirect = () => {
    if (!loading) {
      setLoading(true);
      redirect();
    }
  };

  React.useEffect(() => {
    const timer = setInterval(() => {
      setCount((c) => {
        if (c === 1) {
          clearInterval(timer);
          initiateRedirect();
          return c;
        }
        return c - 1;
      });
    }, 1000);
    return () => {
      clearInterval(timer);
    };
  }, []);

  return (
    <Card>
      <Flex
        justify="between"
        className="w-full"
        data-testid="redirect-countdown"
      >
        <div className="w-3/4">
          <Text as="p">Opening project in {count} seconds...</Text>
        </div>
        <div className="w-1/4">
          <Analytics
            name="one-click-deployment-graphiql-open-project-button"
            passHtmlAttributesToChildren
          >
            <Button
              data-testid="redirect-countdown-redirect-button"
              mode="primary"
              size="1"
              disabled={loading}
              loading={loading}
              loadingText="Redirecting..."
              onClick={initiateRedirect}
            >
              View My Project
            </Button>
          </Analytics>
        </div>
      </Flex>
    </Card>
  );
};
