import React from 'react';
import BootstrapModal from 'react-bootstrap/lib/Modal';
import BootstrapModalButton from 'react-bootstrap/lib/Button';

interface Props {
  show?: boolean;
  title: React.ReactNode;
  onClose: () => void;
  customClass?: string;
  onSubmit?: (() => void) | null;
  onCancel?: (() => void) | null;
  submitText?: string | null;
  submitTestId?: string | null;
  children: React.ReactNode;
}

const Modal: React.FC<Props> = ({
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
      return null;
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
