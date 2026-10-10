import React, { useState } from 'react';
import { Dialog } from '@hasura/shared/ui';
import { BenefitsView } from '../BenefitsView';
import { Form } from './Form';
import { SuccessScreen } from './SuccessScreen/SuccessScreen';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';
import { useQueryClient } from '@tanstack/react-query';

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
    <Dialog size="md" onClose={onFormClose}>
      <FormStateMachine {...props} />
    </Dialog>
  );
}

function FormStateMachine({ onFormClose, showBenefitsView = false }: Props) {
  const queryCLient = useQueryClient();

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
          queryCLient.invalidateQueries({
            queryKey: EE_LICENSE_INFO_QUERY_NAME,
          });
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
