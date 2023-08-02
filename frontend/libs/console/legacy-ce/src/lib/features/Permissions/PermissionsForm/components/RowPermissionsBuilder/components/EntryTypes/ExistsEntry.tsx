import { Entry } from '../Entry';
import { TableProvider } from '../TableProvider';

export function ExistsEntry({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) {
  return (
    <TableProvider>
      <Entry k="_where" v={v._where} path={[...path, '_where']} />
      <Entry k="_table" v={v._table} path={[...path, '_table']} />
    </TableProvider>
  );
}
