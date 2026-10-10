import { useParams } from 'react-router';
import STContainer from '../../components/Container';
import Modify from './Modify';
import { useAppContext } from '@hasura/shared/context';
import { IndicatorCard } from '@hasura/shared/ui';

const ModifyScheduledTrigger = () => {
  const { readOnlyMode } = useAppContext();
  const { triggerName } = useParams<{ triggerName: string }>();
  if (!triggerName) {
    return (
      <IndicatorCard status="negative" showIcon>
        Could not find any cron trigger
      </IndicatorCard>
    );
  }

  return (
    <STContainer tabName="modify" triggerName={triggerName}>
      {({ currentTrigger }) => {
        return readOnlyMode ? (
          'Cannot modify in read-only mode'
        ) : (
          <Modify currentTrigger={currentTrigger} />
        );
      }}
    </STContainer>
  );
};

export default ModifyScheduledTrigger;
