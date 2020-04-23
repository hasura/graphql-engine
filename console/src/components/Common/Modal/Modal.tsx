import React from 'react';
import {
  Modal as BootstrapModal,
  Button as BootstrapModalButton,
} from 'react-bootstrap';

const Modal = ({
  show = true,
  title,
  onClose,
  customClass = '',
  onSubmit,
  onCancel,
  submitText = '',
  submitTestId = '',
  children,
}: {
  show?: boolean;
  title?: string;
  onClose?: Function;
  customClass?: string;
  onSubmit?: () => void;
  onCancel?: () => void;
  submitText?: string;
  submitTestId?: string;
  children?: any;
}) => {
  const getHeader = (): JSX.Element => {
    return (
      <BootstrapModal.Header closeButton>
        <BootstrapModal.Title>{title}</BootstrapModal.Title>
      </BootstrapModal.Header>
    );
  };

  const getBody = (): JSX.Element => {
    return <BootstrapModal.Body>{children}</BootstrapModal.Body>;
  };

  const triggerOnClose = (): void => {
    if (onCancel) {
      onCancel();
    } else if (onClose) {
      onClose();
    }
  };

  const getFooter = (): JSX.Element | null => {
    if (!onSubmit) {
      return null;
    }

    return (
      <BootstrapModal.Footer>
        <BootstrapModalButton onClick={triggerOnClose}>
          Cancel
        </BootstrapModalButton>
        <BootstrapModalButton
          onClick={onSubmit}
          bsStyle="primary"
          data-test={submitTestId}
        >
          {submitText || 'Submit'}
        </BootstrapModalButton>
      </BootstrapModal.Footer>
    );
  };

  const bootstrapModalProps: BootstrapModal.ModalProps = {
    show,
    dialogClassName: customClass,
    onHide() {},
  };

  if (onClose) {
    bootstrapModalProps.onHide = onClose;
  }

  return (
    <BootstrapModal {...bootstrapModalProps}>
      {getHeader()}
      {getBody()}
      {getFooter()}
    </BootstrapModal>
  );
};

export default Modal;
