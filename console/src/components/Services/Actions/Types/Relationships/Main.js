import React from 'react';
import CustomTypesContainer from '../../Containers/CustomTypesContainer';

const Relationships = ({ allTypes, dispatch, ...relProps }) => {
  console.log(relProps);
  return (
    <CustomTypesContainer tabName="relationships" dispatch={dispatch}>
      Hello relationships
    </CustomTypesContainer>
  );
};

export default Relationships;
