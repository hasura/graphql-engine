import * as React from 'react';
import { Button } from '../../../../../new-components/Button';
import { Analytics } from '../../../../Analytics';

type Props = {
  redirect: VoidFunction;
  timeSeconds: number;
};
export const RedirectCountDown: React.VFC<Props> = props => {
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
      setCount(c => {
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
    <div
      className="w-full flex justify-between p-md bg-white border border-solid-slate-300"
      data-testid="redirect-countdown"
    >
      <div className="flex w-3/4 justify-start items-center">
        <p>Opening project in {count} seconds...</p>
      </div>
      <div className="flex w-1/4 justify-end items-center">
        <Analytics
          name="one-click-deployment-graphiql-open-project-button"
          passHtmlAttributesToChildren
        >
          <Button
            data-testid="redirect-countdown-redirect-button"
            mode="primary"
            disabled={loading}
            isLoading={loading}
            loadingText="Redirecting..."
            onClick={initiateRedirect}
          >
            View My Project
          </Button>
        </Analytics>
      </div>
    </div>
  );
};
