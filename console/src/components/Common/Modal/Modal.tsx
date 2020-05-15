import React from 'react';
import {
  Modal as BootstrapModal,
  Button as BootstrapModalButton,
} from 'react-bootstrap';

export interface ModalProps {
  show?: boolean;
  title: React.ReactElement;
  onClose?(): void;
  onSubmit?(): void;
  onCancel?(): void;
  customClass?: string;
  submitText?: string;
  submitTestId?: string;
}
const Modal: React.FC<ModalProps> = ({
  show = true,
  title,
  onClose,
  customClass = '',
  onSubmit,
  onCancel,
  submitText = '',
  submitTestId = '',
  children,
}) => {
  const getHeader = () => {
    return (
      <BootstrapModal.Header closeButton>
        <BootstrapModal.Title>{title}</BootstrapModal.Title>
      </BootstrapModal.Header>
    );
  };

  const getBody = () => {
    return <BootstrapModal.Body>{children}</BootstrapModal.Body>;
  };

  const triggerOnClose = () => {
    if (onCancel) {
      onCancel();
    } else if (onClose) {
      onClose();
    }
  };

  const getFooter = () => {
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
