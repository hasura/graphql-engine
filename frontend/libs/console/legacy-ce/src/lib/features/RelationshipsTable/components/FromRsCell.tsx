import React, { ReactText } from 'react';
import { FaFont } from 'react-icons/fa';
import { FiType } from 'react-icons/fi';

const RsLeafCell = ({ leafName }: { leafName: ReactText }) => (
  <>
    <FaFont
      className="fill-current text-sm text-muted mr-1 p-0"
      title="Field"
    />
    <span className="mr-2">{leafName}</span>
  </>
);

// the desgin mockup was using FA v5, instead of fa-project-diagram, I've used  fa-code-fork from FA v4 for the time being
// this matches with the icon that we show on RS page
// this can be changed once after we upgrade Font Awesome to v5
const FromRsCell = ({
  rsName,
  leafs,
}: {
  rsName: ReactText;
  leafs: ReactText[];
}) => (
  <div className="flex items-center">
    <FiType
      className="fill-current text-sm text-muted mr-1 p-0"
      title="Type"
      style={{ strokeWidth: 4.5 }}
    />
    {rsName}
    <span className="px-2">/</span>
    {leafs.map(i => (
      <RsLeafCell leafName={i} />
    ))}
  </div>
);

export default FromRsCell;
