import React, { useCallback, useEffect, useState } from 'react';

import { getParentNodeByAttribute } from '../../../utils/domFunctions';
import styles from './Dropdown.module.scss';

export type DropdownPosition = 'bottom' | 'right';
export type DropdownOption = {
  /** html inside each row */
  content: React.ReactNode;
  /** An onClick handler for each row. If this is undefined, the row is not clickable */
  onClick?: () => void;
};

interface DropdownListProps {
  options: DropdownOption[];
  dismiss: () => void;
  position: DropdownPosition;
}

const DropdownList: React.VFC<DropdownListProps> = ({
  options,
  dismiss,
  position,
}) => {
  const dropdownPositionStyle =
    position === 'bottom' ? styles.dropdownBottom : styles.dropdownRight;

  return (
    <ul className={`${styles.dropdown_wrapper} ${dropdownPositionStyle}`}>
      {options.map((option, i) => (
        <li key={i}>
          {option.onClick ? (
            <button
              onClick={() => {
                option?.onClick?.();
                dismiss();
              }}
            >
              {option.content}
            </button>
          ) : (
            option.content
          )}
        </li>
      ))}
    </ul>
  );
};

interface DropdownProps {
  /** Tag the component with this keyPrefix. This can be consumed in tests */
  testId: string;
  /** Dropdown button */
  children:
    | React.ReactNode
    | ((args: { onClick: () => void }) => React.ReactNode);
  /** Line items */
  options: any[];
  /** Prefixes keys with the value */
  keyPrefix?: string;
  /** bottom, right (default: right) */
  position?: DropdownPosition;
}

const Dropdown: React.VFC<DropdownProps> = ({
  keyPrefix,
  testId,
  children,
  options,
  position = 'right',
}) => {
  const nodeId = `data-dropdown-element_${testId}`;
  const [isOpen, setIsOpen] = useState(false);

  const toggle = useCallback(() => {
    setIsOpen(_isOpen => !_isOpen);
  }, []);

  const dismissDropdown = useCallback(() => setIsOpen(false), []);

  useEffect(() => {
    const handler = (e: MouseEvent) => {
      const dataElement = getParentNodeByAttribute(e.target, 'data-element');
      if (!dataElement || dataElement.getAttribute('data-element') !== nodeId) {
        setIsOpen(false);
      }
    };
    if (isOpen) {
      document.addEventListener('click', handler);
    }
    return () => {
      if (isOpen) {
        document.removeEventListener('click', handler);
      }
    };
  }, [isOpen, nodeId]);

  return (
    <div
      key={`${keyPrefix}_wrapper`}
      data-test={testId}
      className={styles.data_dropdown_wrapper}
      data-element={nodeId}
    >
      <div
        className={styles.dataDropdown}
        key={`${keyPrefix}_children_wrapper`}
      >
        <div key={`${keyPrefix}_children`}>
          {typeof children === 'function' ? (
            children({ onClick: toggle })
          ) : (
            <button onClick={toggle}>{children}</button>
          )}
        </div>
        {isOpen && (
          <DropdownList
            position={position}
            options={options}
            dismiss={dismissDropdown}
          />
        )}
      </div>
    </div>
  );
};

export default Dropdown;
