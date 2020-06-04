import React, { MouseEvent } from 'react';

const AnalyzeText: React.FC<{
  data: Array<object>;
  activeNode: number;
  onClick: (e: MouseEvent) => void;
}> = ({ data, activeNode, onClick }) => (
  <>
    {data.map((analysis: any, i: number) => {
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
  </>
);

export default AnalyzeText;
