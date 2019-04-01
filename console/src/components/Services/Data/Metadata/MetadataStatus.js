import React from 'react';

const MetadataStatus = ({ metaDataStyles, support, metadata }) => {
  if (!support) {
    return null;
  }

  const inconsistentObjectsTable = () => {
    return (
      <table id="t01">
        <tbody>
          <tr>
            <th>Firstname</th>
            <th>Lastname</th>
            <th>Age</th>
          </tr>
          <tr>
            <td>Eve</td>
            <td>Jackson</td>
            <td>94</td>
          </tr>
        </tbody>
      </table>
    );
  };

  const content = () => {
    if (metadata.inconsistentObjects.length === 0) {
      return (
        <div className={metaDataStyles.content_width}>
          GraphQL Engine metadata is consistent with Postgres
        </div>
      );
    }
    return inconsistentObjectsTable();
  };

  return (
    <div className={metaDataStyles.intro_note}>
      <h4>Metadata Status</h4>
      {content()}
    </div>
  );
};

export default MetadataStatus;
