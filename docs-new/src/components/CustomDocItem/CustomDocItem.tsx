import React, { Fragment } from 'react';
import ActualDocItem from '@theme/DocItem';
import styles from "./CustomDocItem.module.scss";

import GraphQLWithHasuraBanner from "@site/src/components/GraphQLWithHasuraBanner/GraphQLWithHasuraBanner";
import PageHelpful from "@site/src/components/PageHelpful/PageHelpful";

const CustomDocItem = (props) => (
  <div className={styles["custom_doc_layout_wrapper"]}>
    <ActualDocItem {...props} />
    <PageHelpful />
    <GraphQLWithHasuraBanner />
  </div>
)

export default CustomDocItem;
