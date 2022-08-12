import React from 'react';

interface InputProps extends React.ComponentProps<'input'> {
  isAllColumnChecked: boolean;
  handleColumnRadioButton: () => void;
  readOnly: boolean;
}

export const ColumnSelectionRadioButton: React.FC<InputProps> = ({
  isAllColumnChecked,
  handleColumnRadioButton,
  readOnly,
}) => {
  return (
    <div className="mt-sm">
      <label className="radio-inline">
        <input
          type="radio"
          checked={isAllColumnChecked}
          onChange={handleColumnRadioButton}
          disabled={readOnly}
          readOnly
          className="cursor-pointer"
        />
        All columns
      </label>
      <label className="radio-inline">
        <input
          type="radio"
          checked={!isAllColumnChecked}
          onChange={handleColumnRadioButton}
          disabled={readOnly}
          readOnly
          className="cursor-pointer"
          data-test="choose-column"
        />
        Choose columns
      </label>
    </div>
  );
};
