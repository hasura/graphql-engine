import { requestJson } from '@hasura/shared/utils';
import { useAuthContext } from '@hasura/shared/context';
import { useLocation, useNavigate } from 'react-router';
import { HttpError, LOGIN_PATH, UnauthorizedError } from '@hasura/shared/types';

export const useAuthFetchJson = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const { getHeaders } = useAuthContext();

  return async <T>(url: string | URL, config?: RequestInit): Promise<T> => {
    let headers: Record<string, string> = {};
    try {
      headers = await getHeaders();
    } catch {
      if (location.pathname !== LOGIN_PATH) {
        navigate(LOGIN_PATH);
        throw new UnauthorizedError();
      }
    }

    return requestJson<T>(url, {
      ...config,
      headers: {
        ...config?.headers,
        ...headers,
      },
    }).catch((err) => {
      if (
        err instanceof HttpError &&
        err.status === 401 &&
        location.pathname !== LOGIN_PATH
      ) {
        navigate(LOGIN_PATH);
        throw new UnauthorizedError(err.message);
      }

      throw err;
    });
  };
};
