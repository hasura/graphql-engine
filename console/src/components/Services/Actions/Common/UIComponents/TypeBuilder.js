import React from 'react';
import Tabs from '../../../../Common/Tabs';
import ScalarBuilder from './ScalarBuilder';
import ObjectBuilder from './ObjectBuilder';
import InputObjectBuilder from './InputObjectBuilder';

const TypeBuilder = ({ type, setType, allTypes }) => {
  // TODO ENUM

  const scalarBuilder = (
    <ScalarBuilder type={type} setType={setType} allTypes={allTypes} />
  );
  // const enumBuilder = <div>Enum Builder</div>;
  const objectBuilder = (
    <ObjectBuilder type={type} setType={setType} allTypes={allTypes} />
  );
  const inputObjectBuilder = (
    <InputObjectBuilder type={type} setType={setType} allTypes={allTypes} />
  );
  const tabs = [
    {
      heading: 'Scalar',
      component: scalarBuilder,
    },
    {
      heading: 'Object',
      component: objectBuilder,
    },
    {
      heading: 'Input Object',
      component: inputObjectBuilder,
    },
    // {
    //   heading: 'Enum',
    //   component: enumBuilder,
    // },
  ];
  return (
    <Tabs
      tabHeightPx={40}
      activeTabIndex={0}
      marginBottomPx={30}
      tabs={tabs}
      minHeightPx={200}
      tabMarginBottomPx={20}
    />
  );
};

export default TypeBuilder;
