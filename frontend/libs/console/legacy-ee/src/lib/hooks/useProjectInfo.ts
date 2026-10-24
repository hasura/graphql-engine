import { ControlPlane } from '@hasura/console-legacy-ce';
import { useAppContext } from '@hasura/shared/context';
import { useQuery } from '@tanstack/react-query';
import { useMemo } from 'react';
import type { GraphQLError } from 'graphql';
import { getHasuraMetricsUrl } from '../Globals';
import { useAuthContext } from '../shared/auth/context';
import { decodeToken } from '../shared/auth/utils';

// 1 hour
const DEFAULT_STALE_TIME = 60 * 60 * 1000;

export const CLOUD_PROJECT_INFO_QUERY_KEY = 'GET_CLOUD_PROJECT_INFO';

export type CloudProjectInfo = {
  id: string;
  privileges: string[];
  metricsFQDN: string;
  plan_name: string | null;
  entitlements: {
    id: string;
    entitlement: {
      type: ControlPlane.Project_Entitlement_Types_Enum;
      config_is_enabled: boolean;
    };
  }[];
};

export const useProjectInfo = <T = CloudProjectInfo | null>(
  selector?: (m: CloudProjectInfo | { metricsFQDN: string } | null) => T,
  staleTime: number = DEFAULT_STALE_TIME,
) => {
  const { envVars } = useAppContext();
  const { authType, idToken } = useAuthContext();
  // Self-hosted Pro consoles signed in with Hasura get the project and the
  // user's collaborator privileges from the Hasura OAuth id token.
  const tokenClaims = useMemo(() => {
    if (
      (envVars.consoleType !== 'pro' && envVars.consoleType !== 'pro-lite') ||
      authType !== 'hasura-sso' ||
      !idToken
    ) {
      return null;
    }
    try {
      return decodeToken(idToken);
    } catch {
      return null;
    }
  }, [envVars.consoleType, authType, idToken]);
  const queryReturn = useQuery({
    queryKey: [
      CLOUD_PROJECT_INFO_QUERY_KEY,
      tokenClaims?.sub,
      tokenClaims?.project?.id,
      tokenClaims?.collaborator_privileges,
    ],
    queryFn: async () => {
      if (envVars.consoleType === 'oss' || envVars.consoleMode === 'cli') {
        return null;
      }

      if (tokenClaims) {
        const projectId: string | undefined = tokenClaims.project?.id;
        const metricsFQDN = getHasuraMetricsUrl({
          ...envVars,
          hasuraMetricsUrl:
            envVars.hasuraMetricsUrl || tokenClaims.metrics_fqdn,
        });
        return projectId && metricsFQDN
          ? ({
              id: projectId,
              metricsFQDN,
              privileges: tokenClaims.collaborator_privileges ?? [],
              plan_name: tokenClaims.project?.plan_name ?? null,
              entitlements: tokenClaims.project?.entitlements ?? [],
            } as CloudProjectInfo)
          : null;
      }

      if (!envVars.projectID) {
        return null;
      }

      if (envVars.consoleType === 'pro' || envVars.consoleType === 'pro-lite') {
        const hasuraMetricsUrl = getHasuraMetricsUrl(envVars);
        return hasuraMetricsUrl
          ? ({
              id: envVars.projectID,
              metricsFQDN: hasuraMetricsUrl,
            } as CloudProjectInfo)
          : null;
      }

      const response = await ControlPlane.controlPlaneClient.query<
        { data?: ControlPlane.FetchProjectInfoQuery; errors?: GraphQLError[] },
        ControlPlane.FetchProjectInfoQueryVariables
      >(ControlPlane.FETCH_PROJECT_INFO, { id: envVars.projectID });

      if (response.errors?.length) {
        throw new Error(response.errors[0].message);
      }

      const projectQuery = response.data;
      if (!projectQuery?.projects_by_pk) {
        throw new Error('Project not found');
      }

      const project = projectQuery.projects_by_pk;
      const user = projectQuery.users[0];
      const isOwner = project.owner?.id === user.id;
      const projectInfo = {
        id: project.id,
        privileges: isOwner
          ? ['admin', 'graphql_admin', 'view_metrics']
          : (
              project.collaborators.find((c) => c.collaborator?.id === user.id)
                ?.project_collaborator_privileges || []
            ).map((p) => p.privilege_slug),
        metricsFQDN: project.tenant?.region_info?.metrics_fqdn || '',
        plan_name: project?.plan_name,
        entitlements: project?.entitlements,
      };

      return projectInfo;
    },
    staleTime: staleTime || DEFAULT_STALE_TIME,
    refetchOnWindowFocus: false,
    // Route guards block rendering until this settles. Don't retry with
    // backoff, and don't re-run a failed lookup every time a guard mounts,
    // otherwise each navigation into a guarded route stalls for seconds.
    retry: 1,
    retryOnMount: false,
    select: selector,
  });

  return queryReturn;
};
