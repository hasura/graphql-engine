import React from 'react';
import TabContainer from '../Containers/TabContainer';

const Relationships = ({
  params,
  allActions,
  dispatch,
  ...relationshipsProps
}) => {
  console.log(relationshipsProps);
  return (
    <TabContainer
      params={params}
      allActions={allActions}
      tabName="relationships"
      dispatch={dispatch}
    >
      <div> Hello Relationships </div>
    </TabContainer>
  );
};

export default Relationships;
