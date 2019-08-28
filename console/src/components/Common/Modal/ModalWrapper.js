import React from 'react';
import Modal from 'react-bootstrap/lib/Modal';

const ModalWrapper = ({ show, onHide, dialogClassName, title, children }) => {
  return (
    <Modal show={show} onHide={onHide} dialogClassName={dialogClassName}>
      <Modal.Header closeButton>
        <Modal.Title>{title}</Modal.Title>
      </Modal.Header>
      <Modal.Body>{children}</Modal.Body>
    </Modal>
  );
};

export default ModalWrapper;
