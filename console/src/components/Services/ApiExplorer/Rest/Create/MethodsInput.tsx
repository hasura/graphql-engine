import React, { useEffect, useState, ChangeEvent } from 'react';

import { AllowedRESTMethods } from '../../../../../metadata/types';

import styles from '../RESTStyles.scss';

type MethodsInputProps = {
  currentState: AllowedRESTMethods[];
  updateState: (methods: AllowedRESTMethods[]) => void;
};

type InputField = {
  label: AllowedRESTMethods;
  checked: boolean;
};

const defaultInputState: InputField[] = [
  { label: 'GET', checked: false },
  { label: 'POST', checked: false },
  { label: 'PUT', checked: false },
  { label: 'PATCH', checked: false },
  { label: 'DELETE', checked: false },
];

const isAllowedRESTMethod = (v: unknown): v is AllowedRESTMethods => {
  return (
    v === 'GET' ||
    v === 'POST' ||
    v === 'PUT' ||
    v === 'PATCH' ||
    v === 'DELETE'
  );
};

const MethodsInput: React.FC<MethodsInputProps> = ({
  currentState,
  updateState,
}) => {
  const [methodState, updateMethods] = useState(defaultInputState);

  useEffect(() => {
    const currentlySelected = methodState.map(m => {
      if (currentState.includes(m.label)) {
        return { ...m, checked: true };
      }
      return { ...m, checked: false };
    });
    updateMethods(currentlySelected);
  }, [currentState]);

  const onClickCheckBox = (e: ChangeEvent<HTMLInputElement>) => {
    const currentClick = e.currentTarget.value;
    if (!isAllowedRESTMethod(currentClick)) {
      return;
    }
    const isNewClicked = !currentState.includes(currentClick);

    if (isNewClicked) {
      updateState([...currentState, currentClick]);
      return;
    }

    const filteredState = currentState.filter(st => st !== currentClick);
    updateState(filteredState);
  };

  return (
    <div className={styles.methods_layout}>
      <label className={styles.form_input_label}>Methods</label>
      <div className={styles.method_checkboxes}>
        {methodState.map(inp => (
          <div
            key={`rest-method-${inp.label}`}
            className={styles.method_checkbox_layout}
          >
            <input
              type="checkbox"
              checked={inp.checked}
              onChange={onClickCheckBox}
              className="legacy-input-fix"
              value={inp.label}
              id={`rest-method-${inp.label}`}
            />
            <label htmlFor={`rest-method-${inp.label}`}>{inp.label}</label>
          </div>
        ))}
      </div>
    </div>
  );
};

export default MethodsInput;
