import React from 'react';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import { focusYellowRing } from '../constants';

type MigrationCheckboxProps = {
  isChecked: boolean;
  onChange: () => void;
  isCLIMode: boolean;
};

const MigrationCheckbox = ({
  isChecked,
  onChange,
  isCLIMode,
}: MigrationCheckboxProps) =>
  isCLIMode ? (
    <div className="flex items-center">
      <label className="text-sm">
        <input
          type="checkbox"
          checked={isChecked}
          title="This is a migration"
          onChange={onChange}
          className={`${focusYellowRing} !m-0 !mr-sm`}
        />
        This is a migration
        <ToolTip
          placement="right"
          message="Create a migration file with the current insertion"
        />
      </label>
    </div>
  ) : null;

export default MigrationCheckbox;
