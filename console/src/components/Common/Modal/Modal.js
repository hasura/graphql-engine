import React from 'react';
import BootstrapModal from 'react-bootstrap/lib/Modal';
import BootstrapModalButton from 'react-bootstrap/lib/Button';

const Modal = ({
  show = true,
  title,
  onClose,
  customClass = '',
  onSubmit = null,
  onCancel = null,
  submitText = null,
  submitTestId = null,
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

  const getFooter = () => {
    if (!onSubmit) {
      return;
    }

    return (
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
    );
  };

  return (
    <BootstrapModal show={show} onHide={onClose} dialogClassName={customClass}>
      {getHeader()}
      {getBody()}
      {getFooter()}
    </BootstrapModal>
  );
};

export default Modal;
