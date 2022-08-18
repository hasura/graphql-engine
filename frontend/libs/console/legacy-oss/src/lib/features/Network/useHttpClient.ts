import { baseUrl as baseURL } from '@/Endpoints';
import axios from 'axios';

export const useHttpClient = ({
  headers,
}: {
  headers: Record<string, string>;
}) => {
  return axios.create({
    baseURL,
    headers,
  });
};
