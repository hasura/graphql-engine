import React from 'react';
import Toggle from 'react-toggle';

import { ToolTip, Heading, Link } from '../../../../UIKit/atoms';
import styles from '../../../../Common/Common.scss';

const enumCompatibilityDocsUrl =
  'https://hasura.io/docs/1.0/graphql/manual/schema/enums.html#create-enum-table';

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
        <Link href={enumCompatibilityDocsUrl} type="moreInfo">
          See enum table requirements
        </Link>
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
  //         <Link
  //           href={enumCompatibilityDocsUrl}
  //           type="moreInfo"
  //         >
  //           See requirements.
  //         </Link>
  //       </i>
  //     </div>
  //   );
  // };

  return (
    <div>
      <Heading type="subHeading">
        Set table as enum
        <ToolTip
          message={
            'Expose the table values as GraphQL enums in the GraphQL API'
          }
          ml="sm"
          mr="20px"
        />
        <Link type="moreInfo" href={enumCompatibilityDocsUrl}>
          See table requirements
        </Link>
      </Heading>
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
