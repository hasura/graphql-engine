import React from 'react';

const LeftRight = ({ children }) => {
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))' }}>
      <div style={{ paddingRight: '10px' }}>{children[0]}</div>
      <div>{children[1]}</div>
    </div>
  );
};

export default LeftRight;
