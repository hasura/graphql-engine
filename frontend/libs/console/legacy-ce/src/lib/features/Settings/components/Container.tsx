import { Outlet } from 'react-router';
import Sidebar from './Sidebar';
import { PageContainer } from '@hasura/shared/ui';

const helmet = 'Settings | Hasura';

const SettingsContainer = () => {
  return (
    <PageContainer helmet={helmet} leftContainer={<Sidebar />}>
      <Outlet />
    </PageContainer>
  );
};

export default SettingsContainer;
