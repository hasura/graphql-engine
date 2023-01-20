import React from 'react';
import { FaLink, FaPen } from 'react-icons/fa';
import { NeonIcon } from './NeonIcon';

type InputModeToggleProps = {
  showNeonButton: boolean;
  toggleShowNeonButton: VoidFunction;
  disabled?: boolean;
};

export function InputModeToggle(props: InputModeToggleProps) {
  const { showNeonButton, toggleShowNeonButton, disabled } = props;
  const handleClick = () => {
    if (!disabled) toggleShowNeonButton();
  };

  return (
    <div className="mb-xs">
      <span
        onClick={handleClick}
        className={`font-[350] ${
          disabled
            ? 'cursor-not-allowed text-muted'
            : 'cursor-pointer text-cloud-dark hover:text-cloud-darker'
        }`}
      >
        {showNeonButton ? (
          <>
            <FaLink className="mb-1" /> Connect Existing Database
          </>
        ) : (
          <>
            <FaPen className="mb-1" /> Create New Database
            <span className="ml-sm text-gray-600">
              Powered by
              <span className="ml-xs">
                <NeonIcon />
              </span>
            </span>
          </>
        )}
      </span>
    </div>
  );
}
