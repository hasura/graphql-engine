import { useQuery } from '@apollo/react-hooks';
import { exportAsAllowList } from './graphql.queries';

/*
const exportToJson = objectData => {
  const filename = 'export.json';
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
*/

const ExportAllowList = ({ name, projectId, updateStatus, children }) => {
  const variables = {
    name,
    projectId,
  };

  const onError = error => {
    updateStatus(error.toString());
  };
  const { loading, data } = useQuery(exportAsAllowList, {
    fetchPolicy: 'network-only',
    variables: { ...variables },
    onError: onError,
  });
  if (loading) {
    return null;
  }
  return children(data);
};

export default ExportAllowList;
