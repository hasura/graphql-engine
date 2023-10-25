import { isComparator } from './utils';
import { Operator } from './Operator';
import { Comparator } from './Comparator';
import { ConditionalTableProvider } from './EntryTypes/ConditionalTableProvider';

export const Key = ({ k, v, path }: { k: string; v: any; path: string[] }) => {
  if (k === '_where' || k === '_table') {
    return <span className="font-bold">{k}</span>;
  }

  if (isComparator(k)) {
    return (
      <ConditionalTableProvider path={path}>
        <Comparator v={v} comparator={k} path={path} />
      </ConditionalTableProvider>
    );
  }
  return <Operator v={v} operator={k} path={path} />;
};
