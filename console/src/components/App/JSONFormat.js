import React from 'react';

const jsonFormat = json => {
  return (
    <div>
      <pre>{JSON.stringify(json, null, 2)}</pre>
    </div>
  );
};

export default jsonFormat;
