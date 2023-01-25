import React from 'react';

import { useLazyQuery } from '@apollo/react-hooks';
import { exportAsAllowList } from './graphql.queries';
import { ExportList } from '../Common/ExportList';

const exportToJson = objectData => {
  const filename = 'allow-list.json';
  const contentType = 'application/json;charset=utf-8;';
  if (window.navigator && window.navigator.msSaveOrOpenBlob) {
    const blob = new Blob(
      [decodeURIComponent(encodeURI(JSON.stringify(objectData)))],
      { type: contentType }
    );
    navigator.msSaveOrOpenBlob(blob, filename);
  } else {
    const a = document.createElement('a');
    a.download = filename;
    a.href =
      'data:' +
      contentType +
      ',' +
      encodeURIComponent(JSON.stringify(objectData));
    a.target = '_blank';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
  }
};

const ExportAllowList = ({ name, projectId }) => {
  const variables = {
    name,
    projectId,
  };
  const onCompleted = data => {
    if (data.exportAllowList.length > 0) {
      exportToJson(data.exportAllowList);
      return;
    }
  };

  const onError = error => {
    alert(error);
  };
  const [exportAsAllowListQuery, { loading }] = useLazyQuery(
    exportAsAllowList,
    {
      variables: { ...variables },
      onCompleted: onCompleted,
      onError: onError,
      fetchPolicy: 'network-only',
    }
  );

  return (
    <ExportList
      loading={loading}
      onClick={exportAsAllowListQuery}
      altText="Export as allow list"
    />
  );
};

export default ExportAllowList;
