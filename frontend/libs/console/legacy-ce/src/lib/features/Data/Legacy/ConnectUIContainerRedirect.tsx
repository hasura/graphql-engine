import { Navigate, useLocation } from 'react-router';

export const ConnectUIContainerRedirect = ({
  action,
}: {
  action: 'add' | 'edit';
}) => {
  const location = useLocation();

  return (
    <Navigate
      to={
        action === 'add'
          ? {
              pathname: '/data/manage/database/add',
              search: location.search,
            }
          : {
              pathname: '/data/manage/database/edit',
              search: location.search,
            }
      }
    />
  );
};
