import { AuthContext } from '@hasura/shared/context';
import { useContext } from 'react';
import type { EnterpriseAuthService } from './types';

export const useAuthContext = () =>
  useContext(AuthContext) as EnterpriseAuthService;
