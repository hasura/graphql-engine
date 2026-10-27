import { FaCompressAlt, FaExpandAlt } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

/** Styles the expanded editor like the one-line input it replaces. */
export const expandedEditorTw =
  'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder:text-slate-400';

type ExpandToggleProps = {
  expanded: boolean;
  onToggle: () => void;
};

/** The icon over the input's right edge that swaps it for an editor. */
export const ExpandToggle = ({ expanded, onToggle }: ExpandToggleProps) => {
  const Icon = expanded ? FaCompressAlt : FaExpandAlt;
  return (
    <Flex className="cursor-pointer absolute right-2 top-[10px] z-10 h-[14px]">
      <Icon
        className="fill-slate-300 hover:fill-slate-400"
        onClick={onToggle}
        title={expanded ? 'Collapse' : 'Expand'}
      />
    </Flex>
  );
};
