import React from 'react';

import Tabbed from './../../TabbedSchema';
import { useAppSelector } from '../../../../../store';
import { Driver } from '../../../../../dataSources';

import styles from './SchemaGallery.scss';
import { SchemaGalleryBody } from './SchemaGalleryTable';
import { SchemaGalleryModal } from './SchemaGalleryModal';
import { ModalType } from './types';

const SchemaGallery: React.VFC = () => {
  const currentDataSource = useAppSelector<Driver>(
    state => state.tables.currentDataSource
  );

  const [modalState, setShowModal] = React.useState<ModalType | undefined>(
    undefined
  );
  const closeModal = () => setShowModal(undefined);

  return (
    <>
      <Tabbed tabName="gallery" currentDataSource={currentDataSource}>
        <div className={styles.pr_md}>
          <SchemaGalleryBody onModalOpen={setShowModal} />
        </div>
      </Tabbed>
      {modalState !== undefined ? (
        <SchemaGalleryModal closeModal={closeModal} content={modalState} />
      ) : null}
    </>
  );
};
export default SchemaGallery;
