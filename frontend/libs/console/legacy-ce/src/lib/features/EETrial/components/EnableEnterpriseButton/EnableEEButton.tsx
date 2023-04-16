import React, { useState } from 'react';
import { ActivateEEForm } from '../ActivateEEForm';

type Props = {
  children?: React.ReactNode;
  /**
   * Show `View Benefits` button on the success screen.
   */
  showBenefitsView?: boolean;
  /**
   * Trigger when the form is closed
   */
  onFormClose?: VoidFunction;
  /**
   * Disabled state so form will not appear if button is disabled
   */
  disabled?: boolean;
};

/**
 * Component which contains the button wrapper, which start the registration flow.
 * This button should only show up if the user is not registered for EE trial.
 */
export function EnableEEButtonWrapper(props: Props) {
  const {
    children,
    showBenefitsView = false,
    onFormClose = () => {},
    disabled,
  } = props;
  const [showForm, setShowForm] = useState(false);

  return (
    <>
      <div
        role="button"
        onClick={() => {
          if (disabled !== true) {
            setShowForm(true);
          }
        }}
      >
        {children}
      </div>
      {showForm ? (
        <div>
          <ActivateEEForm
            onFormClose={() => {
              setShowForm(false);
              onFormClose();
            }}
            showBenefitsView={showBenefitsView}
          />
        </div>
      ) : null}
    </>
  );
}
