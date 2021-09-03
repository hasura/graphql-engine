import React, { Fragment } from 'react';
import { useAppDispatch, useAppSelector } from '../../../../../store';
import {
  fetchGlobalSchemaSharingConfiguration,
  schemaSharingSelectors,
} from './Actions';
import { currentDriver } from '../../../../../dataSources';
import styles from './TemplateGallery.scss';
import globalStyles from '../../../../Common/Common.scss';
import { modalOpenFn, TemplateGalleryTemplateItem } from './types';

export const TemplateGalleryContentRow: React.VFC<{
  template: TemplateGalleryTemplateItem;
  openModal: () => void;
}> = ({ template, openModal }) => {
  return (
    <tr key={template.key} className={styles.row_content}>
      <td className={styles.td}>
        <a onClick={openModal} className={`${styles.on_hover} text-secondary`}>
          {template.title}
        </a>
      </td>
      <td className={styles.td}>{template.description}</td>
    </tr>
  );
};

export const TemplateGalleryBody: React.VFC<{ onModalOpen: modalOpenFn }> = ({
  onModalOpen,
}) => {
  const dispatch = useAppDispatch();

  const globalStatusFetching = useAppSelector(
    schemaSharingSelectors.getGlobalConfigState
  );
  const templateForDb = useAppSelector(
    schemaSharingSelectors.getSchemasForDb(currentDriver)
  );

  if (globalStatusFetching === 'none') {
    dispatch(fetchGlobalSchemaSharingConfiguration());
  }

  if (globalStatusFetching === 'fetching' || globalStatusFetching === 'none') {
    return (
      <div>
        <span className={`fa fa-spinner fa-spin ${styles.mr_md}`} />
        Loading templates
      </div>
    );
  }

  if (globalStatusFetching === 'failure') {
    return <div>Something went wrong, please try again later.</div>;
  }

  if (!templateForDb || templateForDb.length === 0) {
    return <div>No templates</div>;
  }

  return (
    <>
      <h2
        className={`${globalStyles.heading_text} ${styles.header_table_description} mb-sm`}
      >
        Template Gallery
      </h2>
      <p className={styles.mb_none}>
        Templates are a utility for applying pre-created sets of SQL migrations
        and Hasura metadata.
      </p>
      <p className={styles.mb_none}>
        Below are sets of pre-created templates made to help you get up to speed
        with the functionality of the Hasura platform.
      </p>
      <table className={styles.table}>
        <thead>
          <tr className={styles.gallery_heading_text}>
            <th>Template Name</th>
            <th>Description</th>
          </tr>
        </thead>
        <tbody className={styles.template_table}>
          {templateForDb.map(section => (
            <Fragment key={section.name}>
              <tr key={section.name}>
                <td colSpan={3} className={styles.td}>
                  <span className={`${styles.strong} ${styles.muted}`}>
                    {section.name}
                  </span>
                </td>
              </tr>
              {section.templates.map(template => (
                <TemplateGalleryContentRow
                  key={template.key}
                  template={template}
                  openModal={() =>
                    onModalOpen({ key: template.key, section: section.name })
                  }
                />
              ))}
            </Fragment>
          ))}
        </tbody>
      </table>
      <p className="mb-lg">
        Want to contribute to the official template gallery?{' '}
        <a
          target="_blank"
          rel="noopener noreferrer"
          href="https://github.com/hasura/template-gallery/discussions/2"
        >
          Find our more{' '}
          <i className="fa fa-share-square-o" aria-hidden="true" />
        </a>
      </p>
    </>
  );
};
