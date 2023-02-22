import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';
import { Button } from '../../../../../new-components/Button';
import { Dialog } from '../../../../../new-components/Dialog';
import { CheckboxesField } from '../../../../../new-components/Form';
import React from 'react';
import { FormProvider, useForm } from 'react-hook-form';
import { FaExclamationTriangle } from 'react-icons/fa';

type CustomDialogFooterProps = {
  onSubmit: (formValues: FormValues) => void;
  onClose: () => void;
};

type FormValues = {
  noPermissionsConfirmationDialog: ['enabled'];
};

const CustomDialogFooter: React.FC<CustomDialogFooterProps> = ({
  onClose,
  onSubmit,
}) => {
  const methods = useForm<FormValues>({
    defaultValues: {
      noPermissionsConfirmationDialog: [],
    },
  });

  const onDisable = (formValues: FormValues) => {
    onSubmit(formValues);
  };

  return (
    <div className="flex items-center border-t border-gray-300 bg-white p-sm">
      <div className="flex-grow">
        <FormProvider {...methods}>
          <CheckboxesField
            name="noPermissionsConfirmationDialog"
            options={[{ value: 'enabled', label: "Don't ask me again" }]}
          />
        </FormProvider>
      </div>
      <div className="flex">
        <Button onClick={onClose}>Cancel</Button>
        <div className="ml-2">
          <Button mode="primary" onClick={methods.handleSubmit(onDisable)}>
            Disable
          </Button>
        </div>
      </div>
    </div>
  );
};

export type Props = {
  onSubmit: () => void;
  onClose: () => void;
  title: React.ReactElement;
  description: React.ReactElement;
};

export const PermissionsConfirmationModal: React.FC<Props> = ({
  onSubmit,
  onClose,
  title,
  description,
}) => (
  <Dialog
    title=""
    hasBackdrop
    footer={<CustomDialogFooter onSubmit={onSubmit} onClose={onClose} />}
  >
    <Analytics name="PermissionsConfirmationModal" {...REDACT_EVERYTHING}>
      <div className="flex items-top p-md">
        <div className="text-yellow-500">
          <FaExclamationTriangle className="w-9 h-9 mr-md fill-current" />
        </div>
        <div>
          <p className="font-semibold">{title}</p>
          <div className="overflow-y-auto max-h-[calc(100vh-14rem)]">
            <p className="m-0">{description}</p>
          </div>
        </div>
      </div>
    </Analytics>
  </Dialog>
);
