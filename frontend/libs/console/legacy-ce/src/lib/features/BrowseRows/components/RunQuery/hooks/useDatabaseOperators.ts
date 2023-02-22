import { useState, useEffect } from 'react';
import { DataSource, Feature, Operator } from '../../../../DataSource';
import { useHttpClient } from '../../../../Network';
import { useIsUnmounted } from '../../../../../components/Services/Data/Common/tsUtils';

export const useDatabaseOperators = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const httpClient = useHttpClient();
  const [operators, setOperators] = useState<Operator[]>([]);
  const isUnMounted = useIsUnmounted();

  useEffect(() => {
    async function fetchTableOperators() {
      const result = await DataSource(httpClient).getSupportedOperators({
        dataSourceName,
      });

      if (isUnMounted()) {
        return;
      }

      setOperators(result === Feature.NotImplemented ? [] : result);
    }
    fetchTableOperators();
  }, [dataSourceName, isUnMounted]);

  return operators;
};
