import React, { Fragment } from 'react';
import { useAppDispatch, useAppSelector } from '../../../../../store';
import {
  fetchGlobalSchemaSharingConfiguration,
  schemaSharingSelectors,
  SchemaSharingTemplateItem,
} from './Actions';
import { currentDriver } from '../../../../../dataSources';
import styles from './SchemaGallery.scss';
import { modalOpenFn } from './types';

export const SchemaGalleryContentRow: React.VFC<{
  template: SchemaSharingTemplateItem;
  openModal: () => void;
}> = ({ template, openModal }) => {
  return (
    <tr key={template.key} className={styles.row_content}>
      <td className={styles.td}>
        <a onClick={openModal} className={styles.on_hover}>
          {template.title}
        </a>
      </td>
      <td className={styles.td}>{template.description}</td>
    </tr>
  );
};

export const SchemaGalleryBody: React.VFC<{ onModalOpen: modalOpenFn }> = ({
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
        Loading schemas
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
              <SchemaGalleryContentRow
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
  );
};
