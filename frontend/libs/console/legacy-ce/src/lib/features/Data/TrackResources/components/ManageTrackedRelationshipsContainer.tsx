import { useSuggestedRelationships } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { useTrackedRelationships } from './hooks/useTrackedRelationships';
import { ManageTrackedRelationships } from './ManageTrackedRelationships';

export const ManageTrackedRelationshipsContainer = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const {
    data: trackedFKRelationships,
    isLoading: isLoadingTrackedRelationships,
  } = useTrackedRelationships(dataSourceName);

  const { suggestedRelationships, isLoadingSuggestedRelationships } =
    useSuggestedRelationships({
      dataSourceName,
      existingRelationships: [],
      isEnabled: true,
    });

  if (!suggestedRelationships)
    return <div className="px-md">Something went wrong</div>;

  return (
    <ManageTrackedRelationships
      dataSourceName={dataSourceName}
      suggestedRelationships={suggestedRelationships}
      trackedFKRelationships={trackedFKRelationships}
      isLoading={
        isLoadingSuggestedRelationships || isLoadingTrackedRelationships
      }
    />
  );
};
