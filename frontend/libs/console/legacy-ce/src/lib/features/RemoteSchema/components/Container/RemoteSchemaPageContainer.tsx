import { Outlet } from 'react-router';
import RemoteSchemaSubSidebar from './RemoteSchemaSubSidebar';
import { LeftContainer, PageContainer } from '@hasura/shared/ui';

const RemoteSchemaPageContainer = () => {
  const helmet = 'Remote Schemas | Hasura';

  const leftContainer = (
    <LeftContainer>
      <RemoteSchemaSubSidebar />
    </LeftContainer>
  );

  return (
    <PageContainer helmet={helmet} leftContainer={leftContainer}>
      <Outlet />
    </PageContainer>
  );
};

export default RemoteSchemaPageContainer;
