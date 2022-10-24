import React from 'react';

import { TemplateGalleryBody } from './TemplateGalleryTable';
import { TemplateGalleryModal } from './TemplateGalleryModal';
import { ModalType } from './types';

const TemplateGallery: React.VFC = () => {
  const [modalState, setShowModal] = React.useState<ModalType | undefined>(
    undefined
  );
  const closeModal = () => setShowModal(undefined);

  return (
    <>
      <TemplateGalleryBody onModalOpen={setShowModal} />
      {modalState !== undefined ? (
        <TemplateGalleryModal closeModal={closeModal} content={modalState} />
      ) : null}
    </>
  );
};
export default TemplateGallery;
