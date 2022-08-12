import React from 'react';
import { Spinner } from 'react-bootstrap';

export const LoadingIndicator = () => {
  return (
    <div className="display-flex-center">
      <Spinner animation="border" role="status"/>      
    </div>
  )
};

