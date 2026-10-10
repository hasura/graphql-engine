import { Flex } from '@radix-ui/themes';
import { Key } from './Key';
import { ValueInput } from './ValueInput';

export const EmptyEntry = ({ path }: { path: string[] }) => {
  return (
    <div className="ml-6">
      <Flex gap="4" className="p-2">
        <span className="flex gap-4">
          <Key k={''} path={path} v={null} />
        </span>
        <ValueInput value={''} path={path} />
      </Flex>
    </div>
  );
};
