import { useNavigate } from 'react-router';
import { setLSItem } from '@hasura/shared/utils';
import { LS_KEYS } from '@hasura/shared/types';
import { Button } from './Button';

type Props = {
  dataTestId?: string;
  sql: string;
  customStyles?: string;
  children: React.ReactNode;
  source?: string;
};

export const RawSqlButton = ({
  dataTestId,
  sql,
  customStyles,
  children,
  source,
}: Props) => {
  const navigate = useNavigate();

  return (
    <Button
      data-test={dataTestId}
      className={customStyles}
      size="sm"
      onClick={(e) => {
        e.preventDefault();

        setLSItem(LS_KEYS.rawSQLKey, sql);
        navigate({
          pathname: `/data/sql`,
          search: source ? `source=${source}` : undefined,
        });
      }}
      mode="default"
    >
      {children}
    </Button>
  );
};
