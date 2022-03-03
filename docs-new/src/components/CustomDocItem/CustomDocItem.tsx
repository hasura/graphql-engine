import React from 'react';
import ActualDocItem from '@theme/DocItem';
import GraphQLWithHasuraBanner from "@site/src/components/GraphQLWithHasuraBanner/GraphQLWithHasuraBanner";
import PageHelpful from "@site/src/components/PageHelpful/PageHelpful";
import CustomFooter from '@site/src/components/CustomFooter/CustomFooter';
import styles from "./CustomDocItem.module.scss";

const CustomDocItem = (props) => (
  <div className={`custom_doc_item_wrapper ${styles["custom_doc_item_wrapper"]}`}>
    <ActualDocItem {...props} />
    <div className={styles['custom_doc_item_footer']}>
      <PageHelpful />
      <GraphQLWithHasuraBanner />
      <CustomFooter />
    </div>
  </div>
)

export default CustomDocItem;
