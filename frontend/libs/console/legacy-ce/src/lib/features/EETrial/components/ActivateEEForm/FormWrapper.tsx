import React, { useState } from 'react';
import { Dialog } from '../../../../new-components/Dialog';
import { BenefitsView } from '../BenefitsView';
import { Form } from './Form';
import { SuccessScreen } from './SuccessScreen/SuccessScreen';
import { reactQueryClient } from '../../../../lib/reactQuery';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';

type Props = {
  /**
   * Show `View Benefits` button on the success screen.
   */
  showBenefitsView?: boolean;
  /**
   * Callback for the action to be performed on close of the form
   */
  onFormClose?: VoidFunction;
};

export function FormWrapper(props: Props) {
  const { onFormClose } = props;
  return (
    <Dialog size="md" onClose={onFormClose} hasBackdrop>
      <FormStateMachine {...props} />
    </Dialog>
  );
}

function FormStateMachine(props: Props) {
  const { onFormClose, showBenefitsView = false } = props;

  const [formState, setFormState] = useState<
    'default' | 'successScreen' | 'benefitsScreen'
  >('default');

  if (formState === 'default') {
    return (
      <Form
        onSuccess={() => {
          setFormState('successScreen');
          // on success, invalidate the license status stored in react query cache,
          // overriding the stale time
          reactQueryClient.invalidateQueries(EE_LICENSE_INFO_QUERY_NAME);
        }}
      />
    );
  }

  if (formState === 'successScreen') {
    return (
      <SuccessScreen
        onCloseClick={onFormClose}
        showBenefitsButton={showBenefitsView}
        onViewBenefitsClick={() => {
          setFormState('benefitsScreen');
        }}
      />
    );
  }

  if (formState === 'benefitsScreen') {
    return (
      // TODO: remove hardcoded values
      <BenefitsView
        licenseInfo={{
          status: 'active',
          type: 'trial',
          expiry_at: new Date(new Date().getTime() + 10000000),
          grace_at: new Date(),
        }}
      />
    );
  }

  return null;
}
