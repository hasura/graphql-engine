import { isComparator } from './utils';
import { Operator } from './Operator';
import { Comparator } from './Comparator';
import { useContext } from 'react';
import { tableContext } from './TableProvider';
import { typesContext } from './TypesProvider';
import { Wrapper } from './EntryTypes/utils';

export const Key = ({ k, v, path }: { k: string; v: any; path: string[] }) => {
  const { relationships } = useContext(tableContext);
  const { types } = useContext(typesContext);

  if (k === '_where' || k === '_table') {
    return <span className="font-bold">{k}</span>;
  }

  if (isComparator(k)) {
    return (
      <Wrapper types={types} path={path} relationships={relationships}>
        <Comparator v={v} comparator={k} path={path} />
      </Wrapper>
    );
  }
  return <Operator v={v} operator={k} path={path} />;
};
