import { useQuery } from 'react-query';
import { APIError } from '../../../../hooks/error';
import { fetchAllOnboardingDataQueryFn } from '../utils';
import { onboardingQueryKey } from '../../constants';
import { OnboardingResponseData } from '../../types';

export function useOnboardingData() {
  return useQuery<OnboardingResponseData, APIError>(
    onboardingQueryKey,
    fetchAllOnboardingDataQueryFn,
    {
      staleTime: 5 * 60 * 1000,
      refetchOnWindowFocus: false,
    }
  );
}
