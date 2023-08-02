import React from 'react';
import Toggle from 'react-toggle';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

const enumCompatibilityDocsUrl =
  'https://hasura.io/docs/latest/graphql/core/schema/enums.html#create-enum-table';

export const EnumTableModifyWarning = ({ isEnum }) => {
  if (!isEnum) {
    return null;
  }

  return (
    <div className="rounded bg-yellow-50 border border-gray-300 border-l-4 border-l-yellow-500  py-sm px-md mb-lg">
      <p>
        * This table is set as an enum. Modifying it may cause your Hasura
        metadata to become inconsistent.
        <a
          href={enumCompatibilityDocsUrl}
          target="_blank"
          rel="noopener noreferrer"
          className="ml-xs text-secondary"
        >
          See enum table requirements.
        </a>
      </p>
    </div>
  );
};

const EnumsSection = ({ isEnum, toggleEnum, loading }) => {
  let title;
  if (loading) {
    title = 'Please wait...';
  }

  return (
    <div className="mb-lg">
      <div className="flex items-center mb-formlabel">
        <h4 className="flex items-center text-gray-600 font-semibold">
          Set Table as Enum
          <ToolTip
            message={
              'Expose the table values as GraphQL enums in the GraphQL API'
            }
          />
        </h4>
        <LearnMoreLink
          href={enumCompatibilityDocsUrl}
          text={'(See table requirements)'}
        />
      </div>

      <div title={title} data-toggle="tooltip">
        <Toggle checked={isEnum} icons={false} onChange={toggleEnum} />
      </div>
    </div>
  );
};

export default EnumsSection;
