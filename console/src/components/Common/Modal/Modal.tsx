import React from 'react';
import {
  Modal as BootstrapModal,
  Button as BootstrapModalButton,
} from 'react-bootstrap';
import styles from './Modal.scss';

export interface ModalProps {
  show?: boolean;
  title: React.ReactElement;
  children: React.ReactElement;
  onClose?(): void;
  onSubmit?(): void;
  onCancel?(): void;
  customClass?: string;
  submitText?: React.ReactElement | string;
  leftActions?: React.ReactElement;
  submitTestId?: string;
}
const Modal = ({
  show = true,
  title,
  onClose,
  customClass = '',
  onSubmit,
  onCancel,
  submitText = '',
  leftActions,
  submitTestId = '',
  children,
}: ModalProps) => {
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
        <div className={styles.modal_footer}>
          <div>{leftActions ?? null}</div>

          <div>
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
          </div>
        </div>
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

Modal.Button = BootstrapModalButton;

export default Modal;
