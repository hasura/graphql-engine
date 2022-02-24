import React, { Fragment } from 'react';
import ActualDocPage from '@theme/DocItem';

import GraphQLWithHasuraBanner from "@site/src/components/GraphQLWithHasuraBanner/GraphQLWithHasuraBanner";

const CustomDocLayout = (props) => (
  <div className='custom_doc_layout_wrapper'>
    <ActualDocPage {...props} />
    Custom Doc Item Layout FOOTER - ideally "do u like this page?" kind of stuff.
    <GraphQLWithHasuraBanner />
  </div>
)

export default CustomDocLayout;
