import { useEffect, useState } from 'react';
import { Outlet, useNavigate } from 'react-router';
import { useAuthContext } from '../../shared/auth/context';
import { useProjectInfo } from '../../hooks/useProjectInfo';
import { useAppContext } from '@hasura/shared/context';
import { Privilege } from '@hasura/console-legacy-ce';

type PrivilegesRouteGuardProps = {
  allowedPrivileges: Privilege[];
  deniedRoute: string;
};

// Replaces react-router v3's `onEnter` route lifecycle hook. Renders nothing
// until the hook's callback fires, then renders children - preserving the
// v3 behavior of blocking navigation/render until the guard completes.
const PrivilegesRouteGuard = ({
  allowedPrivileges,
  deniedRoute,
}: PrivilegesRouteGuardProps) => {
  const navigate = useNavigate();
  const [loading, setLoading] = useState(true);
  const { envVars } = useAppContext();
  const { authType, privileges } = useAuthContext();
  const { data: projectInfo, isLoading: projectInfoLoading } = useProjectInfo();

  useEffect(() => {
    if (projectInfoLoading) {
      return;
    }

    /**
     * ## checkPrivileges
     * This function checks if the user is an admin or not and redirects to the access denied page if not.
     * This is used to hide the security tab from non-admin users.
     */
    const checkPrivileges = () => {
      // when consoleType === pro and if admin secret is provided, show security tab
      // when console type is pro-lite only admin secret login is allowed, making this check unnecessary
      // ie. admin privileges are already checked in the login process
      if (envVars.consoleType === 'pro-lite' || envVars.consoleType === 'pro')
        return true; // show security tab

      // cloud cli doesn't have any privileges when `hasura console` command is executed, it will only have previleges when `hasura pro console` is executed.
      // this will make sure that security tab is visible even when the users are running `hasura console` command with valid admin secret
      if (envVars.consoleType === 'cloud' && envVars.consoleMode === 'cli') {
        // this will be true when `hasura console` is executed with a valid admin secret -> through which security tab APIs are accessible
        if (authType !== 'admin-secret') return true; // show security tab
      }

      const userPrivileges = projectInfo?.privileges ?? privileges ?? [];

      return allowedPrivileges.some((ap) => userPrivileges.includes(ap));
    };
    // check privileges for all other cases
    if (!checkPrivileges()) {
      navigate(deniedRoute, { replace: true }); // show access denied page
      return;
    }

    setLoading(false);
  }, [projectInfoLoading]);

  if (loading) return null;
  return <Outlet />;
};

export default PrivilegesRouteGuard;
