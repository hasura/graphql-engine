import React, { useCallback, useState } from 'react';

import { getParentNodeByAttribute } from '../../../utils/domFunctions';
import styles from './Dropdown.scss';

export type DropdownPosition = 'bottom' | 'right';
export type DropdownOption = {
  /** html inside each row */
  content: React.ReactNode;
  /** An onClick handler for each row. If this is undefined, the row is not clickable */
  onClick?: () => void;
};

interface ComponentDataProps {
  options: DropdownOption[];
  dismiss: () => void;
  position: DropdownPosition;
}

const ComponentData: React.VFC<ComponentDataProps> = ({
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
              className={styles.cursorPointer}
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
  const [isOpen, setIsOpen] = useState(false);

  const nodeId = `data-dropdown-element_${testId}`;

  const cb = (d: boolean) => (e: MouseEvent) => {
    /*
     * Update the state only if the element clicked on is not the data dropdown component
     * */
    const dataElement = getParentNodeByAttribute(e.target, 'data-element');
    if (d) {
      /* If the element has parent whose `nodeId` is same as the current one
       * */
      if (dataElement && dataElement.getAttribute('data-element') === nodeId) {
        return;
      }
      setIsOpen(!d);
    }
  };

  const toggle = useCallback(() => {
    /*
     * If the dropdown is not open, attach event on body
     * */
    setIsOpen(!isOpen);

    if (isOpen) {
      document.removeEventListener('click', cb(false));
    } else {
      document.addEventListener('click', cb(true), { once: true });
    }
  }, [isOpen]);

  const dismissDropdown = useCallback(() => setIsOpen(false), []);

  return (
    <div
      key={`${keyPrefix}_wrapper`}
      data-test={`${testId}`}
      className={styles.data_dropdown_wrapper}
      data-element={nodeId}
    >
      <div
        className={styles.dataDropdown}
        key={`${keyPrefix}_children_wrapper`}
      >
        <span key={`${keyPrefix}_children`}>
          {typeof children === 'function' ? (
            children({ onClick: toggle })
          ) : (
            <button onClick={toggle}>{children}</button>
          )}
        </span>
        {isOpen && (
          <ComponentData
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
