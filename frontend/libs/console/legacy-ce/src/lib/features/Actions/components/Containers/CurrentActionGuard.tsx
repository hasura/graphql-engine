import { Outlet } from 'react-router';
import { useCurrentActionContext } from '../../context';
import { IndicatorCard } from '@hasura/shared/ui';

const CurrentActionGuard = () => {
  const { currentAction } = useCurrentActionContext();
  if (!currentAction) {
    return (
      <div className="my-6">
        <IndicatorCard status="negative" showIcon>
          Action not found
        </IndicatorCard>
      </div>
    );
  }

  return <Outlet />;
};

export default CurrentActionGuard;
