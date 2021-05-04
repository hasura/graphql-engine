import React from 'react';
import Toggle from 'react-toggle';
import styles from '../../../../Common/Common.scss';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';

const enumCompatibilityDocsUrl =
  'https://hasura.io/docs/latest/graphql/core/schema/enums.html#create-enum-table';

export const EnumTableModifyWarning = ({ isEnum }) => {
  if (!isEnum) {
    return null;
  }

  return (
    <div className={styles.add_mar_bottom}>
      <i>
        * This table is set as an enum. Modifying it may cause your Hasura
        metadata to become inconsistent.
        <br />
        <a
          href={enumCompatibilityDocsUrl}
          target="_blank"
          rel="noopener noreferrer"
        >
          See enum table requirements.
        </a>
      </i>
    </div>
  );
};

const EnumsSection = ({ isEnum, toggleEnum, loading }) => {
  let title;
  if (loading) {
    title = 'Please wait...';
  }

  // const getCompatibilityNote = () => {
  //   return (
  //     <div>
  //       <i>
  //         * The table must meet some requirements for you to set it as an enum.{' '}
  //         <a
  //           href={enumCompatibilityDocsUrl}
  //           target="_blank"
  //           rel="noopener noreferrer"
  //         >
  //           See requirements.
  //         </a>
  //       </i>
  //     </div>
  //   );
  // };

  return (
    <div>
      <h4 className={`${styles.subheading_text}`}>
        Set table as enum
        <ToolTip
          message={
            'Expose the table values as GraphQL enums in the GraphQL API'
          }
        />
        &nbsp;
        <KnowMoreLink
          href={enumCompatibilityDocsUrl}
          text={'See table requirements'}
        />
      </h4>
      <div
        className={`${styles.display_flex} ${styles.add_mar_bottom}`}
        title={title}
        data-toggle="tooltip"
      >
        <Toggle checked={isEnum} icons={false} onChange={toggleEnum} />
      </div>
    </div>
  );
};

export default EnumsSection;
