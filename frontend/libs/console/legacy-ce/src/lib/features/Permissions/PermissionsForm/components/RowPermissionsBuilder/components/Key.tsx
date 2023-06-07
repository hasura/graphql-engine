import { isComparator } from './utils';
import { Operator } from './Operator';
import { Comparator } from './Comparator';
import { useContext } from 'react';
import { tableContext } from './TableProvider';
import { typesContext } from './TypesProvider';
import { createWrapper } from './EntryTypes/utils';

export const Key = ({ k, v, path }: { k: string; v: any; path: string[] }) => {
  const { relationships } = useContext(tableContext);
  const { types } = useContext(typesContext);

  const Wrapper = createWrapper({
    types,
    path,
    relationships,
  });
  if (k === '_where' || k === '_table') {
    return <span className="font-bold">{k}</span>;
  }

  if (isComparator(k)) {
    return (
      <Wrapper>
        <Comparator v={v} comparator={k} path={path} />
      </Wrapper>
    );
  }
  return <Operator v={v} operator={k} path={path} />;
};
