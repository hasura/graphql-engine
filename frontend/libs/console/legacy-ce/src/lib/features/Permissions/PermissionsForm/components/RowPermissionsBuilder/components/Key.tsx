import { isComparator } from './utils';
import { Operator } from './Operator';
import { Comparator } from './Comparator';

export const Key = ({
  k,
  path,
  noValue,
}: {
  k: string;
  path: string[];
  noValue?: boolean;
}) => {
  if (k === '_where' || k === '_table') {
    return <span className="font-bold">{k}</span>;
  }

  if (isComparator(k)) {
    return <Comparator noValue={noValue} comparator={k} path={path} />;
  }
  return <Operator noValue={noValue} operator={k} path={path} />;
};
