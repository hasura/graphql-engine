import { PermissionsInput } from '../PermissionsInput';
import { EmptyEntry } from '../EmptyEntry';
import { createWrapper } from './utils';
import { useContext } from 'react';
import { isComparator } from '../utils';
import { Token } from '../Token';
import isEmpty from 'lodash/isEmpty';
import isPlainObject from 'lodash/isPlainObject';
import { typesContext } from '../TypesProvider';
import { tableContext } from '../TableProvider';

export function ObjectOrArrayEntry({
  k,
  v,
  path,
}: {
  k: string;
  v: any;
  path: string[];
}) {
  const { types } = useContext(typesContext);
  const { relationships } = useContext(tableContext);

  const Wrapper = createWrapper({
    types,
    path,
    relationships,
  });
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
