import { Progress } from '@radix-ui/themes';
import { useGlobalLoadingStore } from './store';

const ProgressBarWrapper = () => {
  const { requestStatus } = useGlobalLoadingStore();

  const connectionFailMsg =
    requestStatus === 'connection-error' ? (
      <div className="alert alert-danger">
        <strong>
          Hasura console is not able to reach your Hasura GraphQL engine
          instance. Please ensure that your instance is running and the endpoint
          is configured correctly.
        </strong>
      </div>
    ) : null;

  return (
    <>
      {connectionFailMsg}
      {requestStatus === 'ongoing' && (
        <Progress
          aria-label="Loading"
          color="tomato"
          radius="none"
          size="1"
          className="fixed! top-0 left-0 z-[9999] h-0.5! w-full"
        />
      )}
    </>
  );
};

export default ProgressBarWrapper;
