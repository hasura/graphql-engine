import React, { Fragment } from 'react';
import ActualDocPage from '@theme/DocItem';

const CustomDocLayout = (props) => (
  <Fragment>
    <ActualDocPage {...props} />
    Custom Doc Item Layout FOOTER - ideally "do u like this page?" kind of stuff.
  </Fragment>
)

export default CustomDocLayout;