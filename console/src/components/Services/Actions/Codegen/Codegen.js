import React from 'react';
import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import { getAllCodegenFrameworks } from './utils';

const Codegen = ({ allActions, allTypes }) => {
  const [allFrameworks, setAllFrameworks] = React.useState([]);
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);

  console.log(getSdlComplete(allActions, allTypes));
  console.log(allFrameworks);

  React.useEffect(() => {
    getAllCodegenFrameworks()
      .then(frameworks => {
        setAllFrameworks(frameworks);
        setLoading(false);
      })
      .catch(e => {
        console.error(e);
        setError(e);
      });
  }, []);

  if (error) {
    return 'Error';
  }

  if (loading) {
    return 'Loading';
  }

  return 'hello lolz';
};

export default Codegen;
