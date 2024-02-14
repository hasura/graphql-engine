import * as React from 'react';
import { UseQueryResult } from 'react-query';
import { useEELicenseInfo } from '../hooks/useEELicenseInfo';
import { EELicenseInfo } from '../types';

type Props = {
  children: (result: UseQueryResult<EELicenseInfo, unknown>) => React.ReactNode;
};
/*
  This component uses the render-prop pattern to allow using
  the logic from `useEELicenseInfo` hook in React class copmonents
*/
export const WithLicenseInfo = (props: Props) => {
  const licenseInfoResult = useEELicenseInfo();
  return props.children(licenseInfoResult);
};
