import React from 'react';
import BootstrapModal from 'react-bootstrap/lib/Modal';
import BootstrapModalButton from 'react-bootstrap/lib/Button';

const Modal = ({
  show = true,
  title,
  onClose,
  onSubmit,
  onCancel = null,
  submitText = null,
  submitTestId = null,
  children,
}) => {
  return (
    <BootstrapModal show={show} onHide={onClose}>
      <BootstrapModal.Header closeButton>
        <BootstrapModal.Title>{title}</BootstrapModal.Title>
      </BootstrapModal.Header>
      <BootstrapModal.Body>{children}</BootstrapModal.Body>
      <BootstrapModal.Footer>
        <BootstrapModalButton onClick={onCancel || onClose}>
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
    </BootstrapModal>
  );
};

export default Modal;
