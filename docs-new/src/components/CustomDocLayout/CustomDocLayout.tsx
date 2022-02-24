import React, { Fragment } from 'react';
import ActualDocPage from '@theme/DocItem';

import styles from "./CustomDocLayout.module.css";

import GraphQLWithHasuraBanner from "@site/src/components/GraphQLWithHasuraBanner/GraphQLWithHasuraBanner";
import PageHelpful from "@site/src/components/PageHelpful/PageHelpful";

const CustomDocLayout = (props) => (
  <div className={styles["custom_doc_layout_wrapper"]}>
    <ActualDocPage {...props} />
    <PageHelpful />
    <GraphQLWithHasuraBanner />
  </div>
)

export default CustomDocLayout;
