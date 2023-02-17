import { Fragment, useContext, ReactNode } from 'react';
import { get, isEmpty, isPlainObject } from 'lodash';
import { Button } from '@/new-components/Button';
import { isComparator } from './utils/helpers';
import { tableContext, TableProvider } from './TableProvider';
import { typesContext } from './TypesProvider';
import { Token } from './Token';
import { PermissionsInput } from './PermissionsInput';
import { EmptyEntry } from './EmptyEntry';
import { Entry } from './Entry';
import { ValueInput } from './ValueInput';
import { rowPermissionsContext } from './RowPermissionsProvider';

export const EntryType = ({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) => {
  const { types } = useContext(typesContext);
  const { relationships } = useContext(tableContext);

  const { setValue } = useContext(rowPermissionsContext);
  let Wrapper: any = Fragment;
  const type = get(types, path)?.type;

  if (type === 'relationship') {
    const relationship = relationships.find(
      r => r.name === path[path.length - 1]
    );
    if (relationship) {
      const relationshipTable = relationship.table;
      Wrapper = ({ children }: { children?: ReactNode | undefined }) => (
        <TableProvider table={relationshipTable}>{children}</TableProvider>
      );
    }
  }

  switch (k) {
    case '_nin':
    case '_in':
      return (
        <Wrapper>
          <div
            className={
              !isComparator(k) ? `border-dashed border-l border-gray-200` : ''
            }
          >
            {Array.isArray(v) ? (
              <div className="p-2 ml-6">
                {v.map((val, i) => {
                  console.log('path', path, i);
                  return (
                    <ValueInput
                      key={String(i)}
                      value={val}
                      path={[...path, String(i)]}
                    />
                  );
                })}

                <Button
                  onClick={() => setValue([...path, String(v.length)], '')}
                  mode="default"
                >
                  Add input
                </Button>
              </div>
            ) : null}
          </div>
        </Wrapper>
      );

    case '_exists':
      return (
        <TableProvider>
          <Entry k="_where" v={v._where} path={[...path, '_where']} />
          <Entry k="_table" v={v._table} path={[...path, '_table']} />
        </TableProvider>
      );

    default:
      return (
        <Wrapper>
          <div
            className={
              !isComparator(k) ? `border-dashed border-l border-gray-200` : ''
            }
          >
            <PermissionsInput permissions={v} path={path} />
            {Array.isArray(v) && k !== '_table' ? (
              <div className="p-2 ml-6">
                <Token token={'{'} />
                <EmptyEntry path={[...path, `${v.length}`]} />
                <Token token={'}'} />
              </div>
            ) : null}
            {isEmpty(v) && isPlainObject(v) && k !== '_table' ? (
              <EmptyEntry path={path} />
            ) : null}
          </div>
        </Wrapper>
      );
  }
};
