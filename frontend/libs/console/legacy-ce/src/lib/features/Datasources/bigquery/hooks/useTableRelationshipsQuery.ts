import { useQuery } from 'react-query';
import {
  TableRelationshipsType,
  TUseTableRelationshipsQuery,
} from '../../drivers';

export const useTableRelationshipsQuery: TUseTableRelationshipsQuery = ({
  target,
}) => {
  return useQuery(
    `BQ useTableRelationshipsQuery - ${JSON.stringify(target)}`,
    async () => new Promise(res => res([] as TableRelationshipsType[])) // NOTE: BQ has no local relationship support
  );
};
