import React from 'react';

import { TemplateGalleryBody } from './TemplateGalleryTable';
import { TemplateGalleryModal } from './TemplateGalleryModal';
import { ModalType } from './types';
import { SupportedDrivers } from '../../../../../features/hasura-metadata-types';

const TemplateGallery: React.VFC<{
  showHeader?: boolean;
  driver?: SupportedDrivers;
}> = ({ showHeader = true, driver }) => {
  const [modalState, setShowModal] = React.useState<ModalType | undefined>(
    undefined
  );
  const closeModal = () => setShowModal(undefined);

  return (
    <>
      <TemplateGalleryBody
        showHeader={showHeader}
        driver={driver ?? undefined}
        onModalOpen={setShowModal}
      />
      {modalState !== undefined ? (
        <TemplateGalleryModal closeModal={closeModal} content={modalState} />
      ) : null}
    </>
  );
};
export default TemplateGallery;
