import React from 'react';

import { TemplateGalleryBody } from './TemplateGalleryTable';
import { TemplateGalleryModal } from './TemplateGalleryModal';
import { TemplateGalleryTemplateItem } from './types';
import { Source } from '@hasura/shared/types';

const TemplateGallery: React.FC<{
  showHeader?: boolean;
  source: Source;
}> = ({ showHeader = true, source }) => {
  const [modalState, setShowModal] = React.useState<
    TemplateGalleryTemplateItem | undefined
  >(undefined);
  const closeModal = () => setShowModal(undefined);

  return (
    <>
      <TemplateGalleryBody
        showHeader={showHeader}
        source={source}
        onModalOpen={setShowModal}
      />
      {modalState !== undefined ? (
        <TemplateGalleryModal
          closeModal={closeModal}
          currentTemplate={modalState}
          source={source}
        />
      ) : null}
    </>
  );
};
export default TemplateGallery;
