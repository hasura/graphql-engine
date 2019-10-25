import React from 'react';
import Tabs from '../../../../Common/Tabs';
import ScalarBuilder from './ScalarBuilder';
import ObjectBuilder from './ObjectBuilder';
import InputObjectBuilder from './InputObjectBuilder';
import EnumBuilder from './EnumBuilder';

const TypeBuilder = ({ type, setType, argTypes, fieldTypes }) => {
  const scalarBuilder = <ScalarBuilder type={type} setType={setType} />;
  const enumBuilder = <EnumBuilder type={type} setType={setType} />;
  const objectBuilder = (
    <ObjectBuilder
      type={type}
      setType={setType}
      argTypes={argTypes}
      fieldTypes={fieldTypes}
    />
  );
  const inputObjectBuilder = (
    <InputObjectBuilder type={type} setType={setType} argTypes={argTypes} />
  );
  const tabs = [
    {
      heading: 'Scalar',
      component: scalarBuilder,
      key: 'scalar',
    },
    {
      heading: 'Object',
      component: objectBuilder,
      key: 'object',
    },
    {
      heading: 'Input Object',
      component: inputObjectBuilder,
      key: 'input_object',
    },
    {
      heading: 'Enum',
      component: enumBuilder,
      kind: 'enum',
    },
  ];

  const activeTabIndex = tabs.findIndex(t => t.key === type.kind) || 0;

  return (
    <Tabs
      tabHeightPx={40}
      activeTabIndex={activeTabIndex}
      marginBottomPx={30}
      tabs={tabs}
      minHeightPx={200}
      tabMarginBottomPx={20}
    />
  );
};

export default TypeBuilder;
