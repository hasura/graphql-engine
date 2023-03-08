import { baseUrl as baseURL } from '../../Endpoints';
import { ReduxState } from '../../types';
import axios from 'axios';
import { useSelector } from 'react-redux';

export const useHttpClient = () => {
  const headers = useSelector((state: ReduxState) => state.tables.dataHeaders);
  return axios.create({
    baseURL,
    headers,
  });
};
