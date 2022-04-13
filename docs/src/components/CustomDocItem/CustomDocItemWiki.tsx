import React from 'react';
import ActualDocItem from '@theme/DocItem';
import styles from "./styles.module.scss";
import Head from '@docusaurus/Head';

const CustomDocItemWiki = (props) => (
  <div className={`custom_doc_item_wrapper ${styles["custom_doc_item_wrapper"]}`}>
    <Head>
      <meta name="robots" content="noindex" />
    </Head>
    <ActualDocItem {...props} />
  </div>
)

export default CustomDocItemWiki;
