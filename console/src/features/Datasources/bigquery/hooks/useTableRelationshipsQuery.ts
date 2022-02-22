import { TUseTableRelationshipsQuery } from '../../drivers';

export const useTableRelationshipsQuery: TUseTableRelationshipsQuery = () => {
  throw new Error('Big Query does not support foreign Keys');
};
