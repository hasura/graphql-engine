import React from 'react';
import { FaTable } from 'react-icons/fa';

type ExplainPayload = {
  field: string;
  sql: string;
  plan: string[];
};

type RootFieldsProps = {
  data: ExplainPayload[];
  activeNode: number;
  onClick: (e: React.MouseEvent<HTMLLIElement>) => void;
};

const RootFields: React.FC<RootFieldsProps> = ({
  data,
  activeNode,
  onClick,
}) => (
  <ul>
    {data.map((analysis, i) => {
      return (
        analysis.field && (
          // eslint-disable-next-line jsx-a11y/no-noninteractive-element-interactions
          <li
            className={i === activeNode ? 'text-[#fd9540] cursor-pointer' : ''}
            key={i}
            data-key={i}
            onClick={onClick}
          >
            <FaTable
              className="text-sm ml-md mr-2 hover:text-[#fd9540]"
              aria-hidden="true"
            />
            {analysis.field}
          </li>
        )
      );
    })}
  </ul>
);

export default RootFields;
