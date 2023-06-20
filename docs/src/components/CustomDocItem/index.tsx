import React, { useEffect } from 'react';
import ActualDocItem from '@theme/DocItem';
import HasuraConBanner from '@site/src/components/HasuraConBanner';
import GraphQLWithHasuraBanner from '@site/src/components/GraphQLWithHasuraBanner';
import CustomFooter from '@site/src/components/CustomFooter';
import styles from './styles.module.scss';
import { ScrollToFeedbackButton } from '@site/src/components/Feedback/ScrollToFeedbackButton';
import { Redirect } from '@docusaurus/router';

const CustomDocItem = props => {
  useEffect(() => {
    // This function is adds <wbr> tags to code blocks within a table
    // wherever an _ or . is present. We do this since so many environment
    // variables are incredibly long and the word breaks are not happening
    // in a user-friendly way.
    const tables = document.querySelectorAll('table');
    tables.forEach(table => {
      const cells = table.querySelectorAll('td');
      cells.forEach(cell => {
        const codeBlocks = cell.querySelectorAll('code');
        codeBlocks.forEach(codeBlock => {
          codeBlock.innerHTML = codeBlock.innerHTML.replace(/_/g, '_<wbr>');
          codeBlock.innerHTML = codeBlock.innerHTML.replace(/\./g, '.<wbr>');
          console.log(codeBlock.innerHTML);
        });
      });
    });
  }, []);

  // redirect them to the index if they attempt to directly navigate to a path with
  // _heading_ in it
  if (props.location.pathname.includes('_heading_')) {
    return <Redirect to="/docs/latest/index/" />;
  }

  return (
    <div
      className={
        props.location.pathname === `/docs/latest/index/`
          ? `custom_doc_item_wrapper custom_doc_item_wrapper-x-wide`
          : `custom_doc_item_wrapper ${styles['custom_doc_item_wrapper']}`
      }
    >
      <ActualDocItem {...props} />
      <div
        className={
          props.location.pathname === `/docs/latest/index/` || props.location.pathname.includes('overview')
            ? `custom_doc_item_footer-x-wide`
            : styles['custom_doc_item_footer']
        }
      >
        {/*<PageHelpful />*/}
        {props.location.pathname === `/docs/latest/index/` ? null : <ScrollToFeedbackButton />}
        <HasuraConBanner {...props} />
        <GraphQLWithHasuraBanner />
        <CustomFooter />
      </div>
    </div>
  );
};

export default CustomDocItem;
