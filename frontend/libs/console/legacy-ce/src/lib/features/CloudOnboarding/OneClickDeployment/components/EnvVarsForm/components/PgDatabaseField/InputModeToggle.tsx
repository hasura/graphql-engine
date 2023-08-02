import React from 'react';
import { FaLink, FaPen } from 'react-icons/fa';

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
    <span
      onClick={handleClick}
      className={`font-[350] ${
        disabled
          ? 'cursor-not-allowed text-gray-600'
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
        </>
      )}
    </span>
  );
}
