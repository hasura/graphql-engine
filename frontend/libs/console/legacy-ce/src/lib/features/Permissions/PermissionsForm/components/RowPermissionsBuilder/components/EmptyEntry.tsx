import { Key } from './Key';
import { ValueInput } from './ValueInput';

export const EmptyEntry = ({ path }: { path: string[] }) => {
  return (
    <div className="ml-6">
      <div className="p-2 flex gap-4">
        <span className="flex gap-4">
          <Key k={''} path={path} v={null} />
        </span>
        <ValueInput value={''} path={path} />
      </div>
    </div>
  );
};
