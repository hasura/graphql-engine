import { useContext } from 'react';
import { isEmpty } from 'lodash';
import { isPrimitive } from './utils/helpers';
import { tableContext } from './TableProvider';
import { Key } from './Key';
import { Token } from './Token';
import { EntryType } from './EntryType';

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
        <EntryType k={k} v={v} path={path} />
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
