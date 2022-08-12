import React from 'react';

type TableHeaderProps = {
  headings: string[];
};

const TableHeader: React.FC<TableHeaderProps> = props => {
  const { headings } = props;
  return (
    <div>
      <div className="flex bg-[#f0f0f0]">
        {headings.map((heading, index) =>
          heading === 'Inherited Role' ? (
            <div key={index} className="p-xs w-1/3 border font-bold">
              {heading}
            </div>
          ) : (
            <div className="p-xs w-1/3 border border-l-0 font-bold" key={index}>
              {heading}
            </div>
          )
        )}
      </div>
    </div>
  );
};

export default TableHeader;
