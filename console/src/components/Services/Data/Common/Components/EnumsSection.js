import React from 'react';
import Toggle from 'react-toggle';

import { ToolTip, Heading, TextLink, Flex, Box } from '../../../../UIKit/atoms';

const enumCompatibilityDocsUrl =
  'https://hasura.io/docs/1.0/graphql/manual/schema/enums.html#create-enum-table';

export const EnumTableModifyWarning = ({ isEnum }) => {
  if (!isEnum) {
    return null;
  }

  return (
    <Box mb="20px">
      <i>
        * This table is set as an enum. Modifying it may cause your Hasura
        metadata to become inconsistent.
        <br />
        <TextLink href={enumCompatibilityDocsUrl} type="moreInfo">
          See enum table requirements
        </TextLink>
      </i>
    </Box>
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
  //         <TextLink
  //           href={enumCompatibilityDocsUrl}
  //           type="moreInfo"
  //         >
  //           See requirements.
  //         </TextLink>
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
        <TextLink type="moreInfo" href={enumCompatibilityDocsUrl}>
          See table requirements
        </TextLink>
      </Heading>
      <Flex title={title} data-toggle="tooltip" mb="20px">
        <Toggle checked={isEnum} icons={false} onChange={toggleEnum} />
      </Flex>
    </div>
  );
};

export default EnumsSection;
