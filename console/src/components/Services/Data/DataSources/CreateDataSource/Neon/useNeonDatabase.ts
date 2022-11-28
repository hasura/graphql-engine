import React, { useMemo, useCallback } from 'react';
import { createFetchControlPlaneData } from '@/hooks/createFetchControlPlaneData';
import globals from '@/Globals';

type CreateDatabaseResponse = {
  data: {
    neonCreateDatabase: {
      isAuthenticated: boolean;
      databaseUrl?: string;
      email?: string;
    };
  };
};

const NEON_CREATE_DATABASE_QUERY = `
 mutation neonCreateDatabase ($projectId:uuid!) {
    neonCreateDatabase (projectId: $projectId) {
      databaseUrl
      email
      envVar
      isAuthenticated
    }
  }
`;

// this stores the state of the
type NeonDBStatus =
  | {
      status: 'idle';
    }
  | {
      status: 'success';
      payload: CreateDatabaseResponse['data']['neonCreateDatabase'];
    }
  | {
      status: 'error';
      error: 'unauthorized' | string;
    }
  | {
      status: 'loading';
    };

function getHumanReadableAPIError(err: string) {
  if (err.includes('limit exceeded')) {
    return 'You have reached the free tier limit on Neon. Please delete a free tier project from Neon dashboard and try again.';
  }
  return err;
}

export function useNeonDatabase() {
  const [state, setState] = React.useState<NeonDBStatus>({ status: 'idle' });

  // initialise the GraphQL query to create Neon database
  const createNeonDatabase = useMemo(() => {
    return createFetchControlPlaneData<CreateDatabaseResponse>({
      query: NEON_CREATE_DATABASE_QUERY,
      variables: {
        projectId: globals.hasuraCloudProjectId || '',
      },
    });
  }, []);

  // a function to create neon database and set the appropriate state
  const startCreation = useCallback(async () => {
    setState({
      status: 'loading',
    });

    const responseOrError = await createNeonDatabase();
    if (typeof responseOrError === 'string') {
      setState({
        status: 'error',
        error: getHumanReadableAPIError(responseOrError),
      });
    } else {
      const payload = responseOrError.data.neonCreateDatabase;
      if (!payload.isAuthenticated) {
        setState({
          status: 'error',
          error: 'unauthorized',
        });
      } else {
        setState({
          status: 'success',
          payload,
        });
      }
    }
  }, [createNeonDatabase]);

  const reset = useCallback(() => {
    setState({
      status: 'idle',
    });
  }, []);

  return {
    create: startCreation,
    state,
    reset,
  };
}
