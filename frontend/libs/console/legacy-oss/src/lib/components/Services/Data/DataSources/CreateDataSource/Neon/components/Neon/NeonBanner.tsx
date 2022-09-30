import React from 'react';
import { MdRefresh } from 'react-icons/md';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';

const iconMap = {
  refresh: <MdRefresh />,
};

type Status =
  | {
      status: 'loading';
    }
  | {
      status: 'error';
      errorTitle: string;
      errorDescription: string;
    }
  | {
      status: 'default';
    };

export type Props = {
  status: Status;
  onClickConnect: VoidFunction;
  buttonText: string;
  icon?: keyof typeof iconMap;
};

export function NeonBanner(props: Props) {
  const { status, onClickConnect, buttonText, icon } = props;
  const isButtonDisabled = status.status === 'loading';

  return (
    <div className="border border-gray-300 shadow-md rounded bg-white p-md">
      <div className="flex items-center mb-xs">
        <span className="font-semibold flex items-center text-sm py-0.5 px-1.5 text-indigo-600 bg-indigo-100 rounded">
          New
        </span>
        <span className="ml-xs font-semibold flex items-center text-sm py-0.5 px-1.5 text-indigo-600 bg-indigo-100 rounded">
          Free
        </span>
      </div>
      <img
        src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/cloud-console/assets/common/img/neon_banner.png"
        alt="neon_banner"
      />
      <div className="mt-sm mb-sm text-gray-700 text-lg">
        <b>Hasura</b> + <b>Neon</b> are partners now!
      </div>
      <div className="flex justify-between items-center mb-sm">
        <div className="w-3/4 text-md text-gray-700">
          <p>
            The multi-cloud fully managed Postgres with a generous free tier. We
            separated storage and compute to offer autoscaling, branching, and
            bottomless storage.
          </p>
        </div>
        <div className="flex w-1/4 justify-end">
          <Button
            data-trackid="neon-connect-db-button"
            data-testid="neon-connect-db-button"
            mode={status.status === 'loading' ? 'default' : 'primary'}
            isLoading={status.status === 'loading'}
            loadingText={buttonText}
            size="md"
            icon={icon ? iconMap[icon] : undefined}
            onClick={() => {
              if (!isButtonDisabled) {
                onClickConnect();
              }
            }}
            disabled={isButtonDisabled}
          >
            {props.buttonText}
          </Button>
        </div>
      </div>
      {status.status === 'error' && (
        <IndicatorCard status="negative" headline={status.errorTitle} showIcon>
          {status.errorDescription}
        </IndicatorCard>
      )}
    </div>
  );
}
