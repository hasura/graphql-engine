import { Outlet, useLocation, useParams } from 'react-router';
import ActionLeftSidebar from '../LeftSidebar';
import { useMetadata } from '@hasura/metadata/api';
import { CurrentActionContext } from '../../context';
import { findAction } from '../../utils';
import {
  IndicatorCard,
  LeftContainer,
  LeftSidebar,
  SkeletonList,
  PageContainer,
} from '@hasura/shared/ui';
import { dataRoutes } from '@hasura/shared/utils';

const ActionsContainer = () => {
  const location = useLocation();
  const params = useParams();
  const { data: meta, isLoading, error } = useMetadata();

  if (isLoading && !meta) {
    return <SkeletonList count={5} />;
  }

  if (error || !meta) {
    return (
      <IndicatorCard status="negative">
        Error when fetching metadata. Retry again later
      </IndicatorCard>
    );
  }

  const sidebarContent = (
    <LeftSidebar
      items={[
        {
          isActive: location.pathname.includes('actions/manage'),
          label: 'Manage Actions',
          to: dataRoutes.manageActions,
          children: (
            <ActionLeftSidebar
              actions={meta?.metadata.actions ?? []}
              currentAction={params.actionName}
              allowOpenApiImport
            />
          ),
        },
        {
          isActive: location.pathname.includes('actions/types'),
          label: 'Custom types',
          to: dataRoutes.actionTypes(),
        },
      ]}
    />
  );

  const helmet = 'Actions | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;
  const currentAction = params.actionName
    ? findAction(meta.metadata.actions, params.actionName)
    : undefined;

  return (
    <CurrentActionContext.Provider
      value={{
        ...meta,
        currentAction,
      }}
    >
      <PageContainer helmet={helmet} leftContainer={leftContainer}>
        <Outlet />
      </PageContainer>
    </CurrentActionContext.Provider>
  );
};

export default ActionsContainer;
