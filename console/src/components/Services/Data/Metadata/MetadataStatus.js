import React from 'react';

const MetadataStatus = ({ metaDataStyles, support }) => {
  if (!support) {
    return null;
  }

  return (
    <div className={metaDataStyles.intro_note}>
      <h4>Metadata Status</h4>
      <div className={metaDataStyles.content_width}>
        GraphQL Engine metadata is consistent with Postgres
      </div>
    </div>
  );
};

export default MetadataStatus;
