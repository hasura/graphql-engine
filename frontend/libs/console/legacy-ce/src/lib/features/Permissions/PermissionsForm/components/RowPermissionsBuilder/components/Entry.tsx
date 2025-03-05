import { useContext } from 'react';
import isEmpty from 'lodash/isEmpty';
import { isComparator, isPrimitive } from './utils/helpers';
import { tableContext } from './TableProvider';
import { Key } from './Key';
import { Token } from './Token';
import { EntryType } from './EntryType';
import { useOperators } from './utils/comparatorsFromSchema';
import { InputSuggestion } from './InputSuggestion';

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
  const operators = useOperators({ path });
  const operator = operators.find(o => o.name === k);
  if (
    operator?.name === '_contains' ||
    operator?.name === '_contained_in' ||
    operator?.type === 'geometric' ||
    operator?.type === 'geometric_geographic'
  ) {
    return (
      <div
        style={{ marginLeft: 8 + (path.length + 1) * 4 + 'px' }}
        className={`my-2 ${isDisabled ? 'bg-gray-50' : ''}`}
      >
        <div className={`p-2 ${isPrimitive(v) ? ' flex gap-4' : ''}`}>
          <span className="flex gap-4">
            <Key k={k} path={path} v={v} />
            <span>: </span>
            <EntryType k={k} v={v} path={path} />
          </span>
        </div>
      </div>
    );
  }

  return (
    <div
      style={{ marginLeft: 8 + (path.length + 1) * 4 + 'px' }}
      className={`my-2 ${isDisabled ? 'bg-gray-50' : ''}`}
    >
      <div className={`p-2 ${isPrimitive(v) ? ' flex gap-4' : ''}`}>
        <span className="flex gap-4">
          <Key k={k} path={path} v={v} />
          <span>: </span>
          <OpenToken v={v} />
        </span>
        <EntryType k={k} v={v} path={path} />
        <EndToken v={v} path={path} k={k} />
      </div>
    </div>
  );
};

function OpenToken({ v }: { v: any }) {
  return Array.isArray(v) ? (
    <Token token={'['} inline />
  ) : isPrimitive(v) ? null : (
    <Token token={'{'} inline />
  );
}

function EndToken({ v, path, k }: { v: any; path: string[]; k: string }) {
  return Array.isArray(v) ? (
    <div className="flex gap-2 pt-2 items-center">
      <Token token={']'} inline />
      <Token token={','} inline />
      {isComparator(k)?.name === '_in' || isComparator(k)?.name === '_nin' ? (
        <InputSuggestion
          comparatorName={path[path.length - 1]}
          componentLevelId={`${path.join('.')}-value-input`}
          path={path}
        />
      ) : null}
    </div>
  ) : isPrimitive(v) ? null : (
    <div className="flex gap-2">
      <Token token={'}'} inline />
      <Token token={','} inline />
    </div>
  );
}
