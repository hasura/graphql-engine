import { Outlet } from 'react-router';
import { PageContainer } from '@hasura/shared/ui';
import { SIDEBAR_ID } from '../DataSidebar/constants';
import LegacyDataSidebar from '../DataSidebar/LegacyDataSidebar';

const DataPageContainer = () => {
  const leftContainer = (
    <div id={SIDEBAR_ID}>
      <LegacyDataSidebar />
    </div>
  );

  return (
    <PageContainer leftContainer={leftContainer}>
      <Outlet />
    </PageContainer>
  );
};

export default DataPageContainer;
