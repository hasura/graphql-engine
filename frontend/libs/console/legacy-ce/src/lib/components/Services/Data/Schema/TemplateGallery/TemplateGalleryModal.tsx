import React, { useEffect } from 'react';
import { FaGithub, FaSpinner, FaUpload } from 'react-icons/fa';
import { useAppDispatch, useAppSelector } from '../../../../../storeHooks';
import {
  applyTemplate,
  fetchSchemaConfigurationByName,
  schemaSharingSelectors,
} from './Actions';
import Modal from '../../../../Common/Modal/Modal';
import styles from './TemplateGallery.module.scss';
import { ModalType } from './types';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import { showErrorNotification } from '../../../Common/Notification';
import clsx from 'clsx';

export const TemplateGalleryModalBody: React.VFC<{
  content: ModalType;
}> = ({ content }) => {
  const dispatch = useAppDispatch();

  const currentTemplate = useAppSelector(
    schemaSharingSelectors.getTemplateBySectionAndKey({
      section: content.section,
      key: content.key,
    })
  );

  useEffect(() => {
    if (currentTemplate && currentTemplate.fetchingStatus === 'none') {
      dispatch(
        fetchSchemaConfigurationByName({
          key: content.key,
          category: content.section,
        })
      );
    }
  }, [content, currentTemplate, dispatch]);

  if (!currentTemplate) {
    return <div>Something went wrong, please try again later.</div>;
  }

  if (
    currentTemplate.fetchingStatus === 'fetching' ||
    currentTemplate.fetchingStatus === 'none'
  ) {
    return (
      <div>
        <FaSpinner className={`animate-spin ${styles.mr_md}`} />
        Loading template
      </div>
    );
  }
  const details = currentTemplate.details;
  if (currentTemplate.fetchingStatus === 'failure' || !details) {
    return <div>Something went wrong, please try again later.</div>;
  }

  return (
    <div className={styles.modal_body}>
      {details.longDescription ? (
        <p className={styles.description_whitespace}>
          {details.longDescription}
        </p>
      ) : null}
      {details.blogPostLink ? (
        <p className="border-b border-gray-300 pb-3">
          Read the blog post{' '}
          <a
            href={details.blogPostLink}
            target="_blank"
            rel="noopener noreferrer"
          >
            here
          </a>
          .
        </p>
      ) : null}
      {details.imageUrl ? (
        <p className="py-3">
          <img
            className={styles.image_in_detail}
            src={details.imageUrl}
            alt=""
          />
        </p>
      ) : null}
      <p
        className={`${styles.mb_xs} ${styles.strong} ${styles.muted} border-t border-gray-300 pt-4`}
      >
        SQL:
      </p>
      <AceEditor
        readOnly
        value={details.sql}
        className={clsx(styles.ace_custom_style, 'bg-white')}
        mode="sql"
        width="100%"
        showGutter={false}
        showPrintMargin={false}
        setOptions={{ showLineNumbers: false, useWorker: false }}
        maxLines={150}
      />
    </div>
  );
};

export const TemplateGalleryModal: React.VFC<{
  content: ModalType;
  closeModal: () => void;
}> = ({ content, closeModal }) => {
  const dispatch = useAppDispatch();
  const currentTemplate = useAppSelector(
    schemaSharingSelectors.getTemplateBySectionAndKey({
      section: content.section,
      key: content.key,
    })
  );

  useEffect(() => {
    if (currentTemplate === undefined) {
      closeModal();
    }
  }, [closeModal, currentTemplate]);

  if (currentTemplate === undefined) {
    return <div />;
  }

  const onSubmit = () => {
    dispatch(applyTemplate({ key: content.key, category: content.section }))
      .then(() => {
        closeModal();
      })
      .catch(() => {
        dispatch(
          showErrorNotification(
            'An error occurred while applying this template'
          )
        );
      });
  };

  const shouldDisplaySubmit = currentTemplate.fetchingStatus === 'success';

  return (
    <Modal
      show
      title={<p>{currentTemplate.title}</p>}
      onClose={closeModal}
      onCancel={closeModal}
      onSubmit={shouldDisplaySubmit ? onSubmit : undefined}
      submitText={
        shouldDisplaySubmit ? (
          <>
            <FaUpload className={styles.icon_padding} aria-hidden />
            Install Template
          </>
        ) : undefined
      }
      leftActions={
        shouldDisplaySubmit ? (
          <Modal.Button
            onClick={() =>
              window.open(currentTemplate?.details?.publicUrl, '_blank')
            }
          >
            <FaGithub style={{ marginRight: '6px' }} />
            View on GitHub
          </Modal.Button>
        ) : undefined
      }
    >
      <TemplateGalleryModalBody content={content} />
    </Modal>
  );
};
