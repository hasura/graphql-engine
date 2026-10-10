import { useEffect, useState } from 'react';
import { Outlet, useNavigate } from 'react-router';
import { useAuthContext } from '../../shared/auth/context';
import { useProjectInfo } from '../../hooks/useProjectInfo';
import { relativeModulePath } from '../Services/Metrics/constants';
import {
  checkAccess,
  restrictedPathsMetadata,
} from '@hasura/console-legacy-ce';

const DataRouteGuard = () => {
  const navigate = useNavigate();
  const [loading, setLoading] = useState(true);
  const { privileges } = useAuthContext();
  const { data: projectInfo, isLoading: projectInfoLoading } = useProjectInfo();

  useEffect(() => {
    if (projectInfoLoading) {
      return;
    }

    const userPrivileges = projectInfo?.privileges ?? privileges ?? [];
    const accessState = checkAccess(userPrivileges);

    for (let i = Object.keys(restrictedPathsMetadata).length - 1; i >= 0; i--) {
      const restrictedPath = Object.keys(restrictedPathsMetadata)[i];
      const restrictedPathData = restrictedPathsMetadata[restrictedPath];
      if (location.pathname.indexOf(restrictedPath) === 0) {
        if (!accessState[restrictedPathData.keyInAccessState]) {
          navigate(restrictedPathData.replace, { replace: true });
          return;
        }
      }
    }

    if (
      'hasMetricAccess' in accessState &&
      accessState.hasMetricAccess &&
      !('hasDataAccess' in accessState && accessState.hasDataAccess) &&
      !('hasGraphQLAccess' in accessState && accessState.hasGraphQLAccess)
    ) {
      if (
        location.pathname.indexOf(relativeModulePath) === -1 &&
        location.pathname.indexOf('/access-denied') === -1
      ) {
        navigate(relativeModulePath, { replace: true });
      }

      return;
    }

    setLoading(false);
  }, [projectInfoLoading]);

  if (loading) return null;
  return <Outlet />;
};

export default DataRouteGuard;
