import React from 'react';

type ExplainPayload = {
  field: string;
  sql: string;
  plan: string[];
};

type RootFieldsProps = {
  data: ExplainPayload[];
  activeNode: number;
  onClick: (e: React.MouseEvent<HTMLDivElement>) => void;
};

const AnalyzeText: React.FC<RootFieldsProps> = ({
  data,
  activeNode,
  onClick,
}) => (
  <ul>
    {data.map((analysis, i) => {
      return (
        analysis.field && (
          <div
            className={i === activeNode ? 'active' : ''}
            key={i}
            data-key={i}
            onClick={onClick}
            role="link"
          >
            <i className="fa fa-table" aria-hidden="true" />
            {analysis.field}
          </div>
        )
      );
    })}
  </ul>
);

export default AnalyzeText;
