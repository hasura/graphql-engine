import { Outlet } from 'react-router';
import TopBar from './TopNav';
import { useDocumentTitle } from '@hasura/shared/hooks';

const ApiContainer = () => {
  useDocumentTitle('API Explorer | Hasura');

  return (
    <>
      <div id="left-bar">
        <TopBar />
      </div>
      <div id="right-bar">
        <Outlet />
      </div>
    </>
  );
};

export default ApiContainer;
