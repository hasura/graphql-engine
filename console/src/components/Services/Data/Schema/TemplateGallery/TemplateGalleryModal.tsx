import React, { useEffect } from 'react';
import { useAppDispatch, useAppSelector } from '../../../../../store';
import {
  applyTemplate,
  fetchSchemaConfigurationByName,
  schemaSharingSelectors,
} from './Actions';
import Modal from '../../../../Common/Modal/Modal';
import styles from './TemplateGallery.scss';
import { ModalType } from './types';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import { showErrorNotification } from '../../../Common/Notification';

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
        <span className={`fa fa-spinner fa-spin ${styles.mr_md}`} />
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
        <p>
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
        <>
          <hr className={styles.modal_description} />
          <p>
            <img
              className={styles.image_in_detail}
              src={details.imageUrl}
              alt=""
            />
          </p>
        </>
      ) : null}
      <hr className={styles.modal_description} />
      <p className={`${styles.mb_xs} ${styles.strong} ${styles.muted}`}>SQL:</p>
      <AceEditor
        readOnly
        value={details.sql}
        className={styles.ace_custom_style}
        mode="sql"
        width="100%"
        showGutter={false}
        showPrintMargin={false}
        setOptions={{ showLineNumbers: false }}
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
      customClass={styles.modal_override}
      submitText={
        shouldDisplaySubmit ? (
          <>
            <span
              className={`fa fa-upload ${styles.icon_padding}`}
              aria-hidden
            />
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
            <i className="fa fa-github" style={{ marginRight: '6px' }} />
            View on GitHub
          </Modal.Button>
        ) : undefined
      }
    >
      <TemplateGalleryModalBody content={content} />
    </Modal>
  );
};
