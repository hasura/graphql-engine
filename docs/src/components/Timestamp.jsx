import React from 'react';

// This component is for the GraphQL Schema quickstart wherein we create a coupon that needs a timestamp in the future for the expiration date.
const Timestamp = () => {
  const date = new Date();
  date.setDate(date.getDate() + 14);
  date.setHours(0, 0, 0, 0);
  return <code>{date.toISOString().slice(0, 19).replace('T', ' ')}</code>;
};

export default Timestamp;
