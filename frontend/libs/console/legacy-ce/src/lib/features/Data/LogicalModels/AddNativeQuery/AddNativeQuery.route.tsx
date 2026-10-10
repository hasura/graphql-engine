import { RouteWrapper } from '../components/RouteWrapper';
import { AddNativeQuery } from './AddNativeQuery';
import { Routes } from '../constants';

export const AddNativeQueryRoute = () => {
  return (
    <RouteWrapper route={Routes.CreateNativeQuery}>
      <AddNativeQuery />
    </RouteWrapper>
  );
};
