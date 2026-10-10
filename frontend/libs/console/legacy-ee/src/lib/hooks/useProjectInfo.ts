import { ControlPlane } from '@hasura/console-legacy-ce';
import { useAppContext } from '@hasura/shared/context';
import { useQuery } from '@tanstack/react-query';
import { getHasuraMetricsUrl } from '../Globals';

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
  const queryReturn = useQuery({
    queryKey: [CLOUD_PROJECT_INFO_QUERY_KEY],
    queryFn: async () => {
      if (
        envVars.consoleType === 'oss' ||
        envVars.consoleMode === 'cli' ||
        !envVars.projectID
      ) {
        return null;
      }

      if (envVars.consoleType === 'pro' || envVars.consoleType === 'pro-lite') {
        const hasuraMetricsUrl = getHasuraMetricsUrl(envVars);
        return hasuraMetricsUrl
          ? ({
              id: envVars.projectID,
              metricsFQDN: envVars.hasuraMetricsUrl,
            } as CloudProjectInfo)
          : null;
      }

      const projectQuery = await ControlPlane.controlPlaneClient.query<
        ControlPlane.FetchProjectInfoQuery,
        ControlPlane.FetchProjectInfoQueryVariables
      >(ControlPlane.FETCH_PROJECT_INFO, { id: envVars.projectID });

      if (!projectQuery.projects_by_pk) {
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
    select: selector,
  });

  return queryReturn;
};
