import { createContext, useContext } from 'react';
import { getDatabaseItemTreeId, getSourceTreeId } from './NavTree/selectors';

type SidebarContextType = {
  // data sources:
  loadingSources: string[];

  //can be tables or functions;
  loadingItems: string[];
};

export const SidebarContext = createContext<SidebarContextType | undefined>(
  undefined
);

export const useIsDataSourceLoading = (dataSourceName: string) => {
  const context = useContext(SidebarContext);

  if (context === undefined) {
    throw new Error(
      'useIsDataSourceLoading must be used within a SidebarProvider'
    );
  }

  return context.loadingSources.includes(getSourceTreeId(dataSourceName));
};

export const useIsDatabaseItemLoading = ({
  dataSourceName,
  item,
  type,
}: {
  dataSourceName: string;
  item: unknown;
  type: 'table' | 'function';
}) => {
  const context = useContext(SidebarContext);

  if (context === undefined) {
    throw new Error(
      'useIsDatabaseItemLoading must be used within a SidebarProvider'
    );
  }

  const { loadingSources, loadingItems } = context;

  const sourceId = getSourceTreeId(dataSourceName);
  const itemId = getDatabaseItemTreeId({ dataSourceName, item, type });
  return loadingSources.includes(sourceId) && loadingItems.includes(itemId);
};

export const useIsAnythingLoading = () => {
  const context = useContext(SidebarContext);

  if (context === undefined) {
    throw new Error(
      'useIsAnythingLoading must be used within a SidebarProvider'
    );
  }

  return context.loadingItems.length > 0 || context.loadingSources.length > 0;
};
