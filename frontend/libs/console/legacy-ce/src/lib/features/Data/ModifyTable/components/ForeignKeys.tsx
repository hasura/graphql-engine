import React from 'react';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import Skeleton from 'react-loading-skeleton';
import { ModifyTableProps } from '../ModifyTable';
import { ForeignKeyDescription } from './ForeignKeyDescription';
import { useTableForeignKeys } from '../hooks/useTableForeignKeys';

interface ForeignKeysProps extends ModifyTableProps {}
export const ForeignKeys: React.VFC<ForeignKeysProps> = props => {
  const { dataSourceName, table } = props;
  const { data, isLoading, isError } = useTableForeignKeys({
    dataSourceName,
    table,
  });
  const foreignKeys = data?.foreignKeys ?? [];

  if (isLoading || !foreignKeys) return <Skeleton count={5} height={20} />;

  if (isError)
    return (
      <IndicatorCard status="negative" headline="error">
        Unable to fetch foreign keys
      </IndicatorCard>
    );

  if (!foreignKeys.length) {
    // Some databases don't support foreign keys
    return null;
  }

  return (
    <>
      {foreignKeys.map(foreignKey => (
        <ForeignKeyDescription foreignKey={foreignKey} />
      ))}
    </>
  );
};
