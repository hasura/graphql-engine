import { Fragment, useContext, ReactNode } from 'react';
import { get, isEmpty, isPlainObject } from 'lodash';
import { isComparator, isPrimitive } from './utils/helpers';
import { tableContext, TableProvider } from './TableProvider';
import { typesContext } from './TypesProvider';
import { Key } from './Key';
import { Token } from './Token';
import { PermissionsInput } from './PermissionsInput';
import { EmptyEntry } from './EmptyEntry';

export const Entry = ({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) => {
  const { table } = useContext(tableContext);
  const isDisabled = k === '_where' && isEmpty(table);
  const { types } = useContext(typesContext);
  const { relationships } = useContext(tableContext);
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

  return (
    <div
      style={{ marginLeft: 8 + (path.length + 1) * 4 + 'px' }}
      className={`my-2 ${isDisabled ? 'bg-gray-50' : ''}`}
    >
      <div className={`p-2 ${isPrimitive(v) ? ' flex gap-4' : ''}`}>
        <span className="flex gap-4">
          <Key k={k} path={path} />
          <span>: </span>
          {Array.isArray(v) ? (
            <Token token={'['} inline />
          ) : isPrimitive(v) ? null : (
            <Token token={'{'} inline />
          )}
        </span>
        {k === '_exists' ? (
          <TableProvider>
            <Entry k="_where" v={v._where} path={[...path, '_where']} />
            <Entry k="_table" v={v._table} path={[...path, '_table']} />
          </TableProvider>
        ) : (
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
        )}
        {Array.isArray(v) ? (
          <div className="flex gap-2">
            <Token token={']'} inline />
            <Token token={','} inline />
          </div>
        ) : isPrimitive(v) ? null : (
          <div className="flex gap-2">
            <Token token={'}'} inline />
            <Token token={','} inline />
          </div>
        )}
      </div>
    </div>
  );
};
