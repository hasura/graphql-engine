import React from 'react';

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
            className={i === activeNode ? 'active' : ''}
            key={i}
            data-key={i}
            onClick={onClick}
          >
            <i className="fa fa-table" aria-hidden="true" />
            {analysis.field}
          </li>
        )
      );
    })}
  </ul>
);

export default RootFields;
