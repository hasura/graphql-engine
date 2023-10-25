import { ModelSummary } from './ModelSummary';
import { useModelCountSummary } from './useModelCountSummary';

export const ModelSummaryContainer = () => {
  const {
    data: { tablesAndViews = [], logicalModels = [], collections = [] } = {},
    isLoading,
  } = useModelCountSummary();

  if (isLoading) return <>Loading...</>;

  return (
    <ModelSummary
      tablesAndViews={tablesAndViews}
      logicalModels={logicalModels}
      collections={collections}
    />
  );
};
