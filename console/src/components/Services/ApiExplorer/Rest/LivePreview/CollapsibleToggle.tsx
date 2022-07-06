import React, { useState } from 'react';
import { FaChevronRight } from 'react-icons/fa';

type CollapsibleToggleProps = {
  state: Record<string, any>[];
  properties: string[];
  title: string;
};

const CollapsibleToggle: React.FC<CollapsibleToggleProps> = ({
  title,
  state,
  properties,
  children,
}) => {
  const [isOpen, setIsOpen] = useState(true);

  const toggleHandler = () => setIsOpen(prev => !prev);

  return (
    <div className={`rounded-sm p-md ${!isOpen ? ' bg-gray-100' : ''}`}>
      <div
        className="cursor-pointer flex items-center"
        onClick={toggleHandler}
        role="button"
        tabIndex={0}
      >
        <span className="text-base pr-sm">
          <FaChevronRight
            className={`transition duration-150 ease-in-out ${
              isOpen && ' rotate-90'
            }`}
          />
        </span>

        <span className="flex items-center">
          <div className="font-base font-bold text-gray-500">{title}</div>
        </span>
      </div>

      {isOpen ? (
        <>{children}</>
      ) : (
        <div className="flex flex-wrap break-words">
          {state?.map((stateVar, index) => (
            <div className="flex">
              <div className="overflow-hidden text-ellipsis max-w-[300] text-base">
                {`${stateVar[properties[0]]}  :  ${stateVar[properties[1]]}`}
              </div>
              {index !== state?.length - 1 ? <p>|</p> : null}
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

export default CollapsibleToggle;
