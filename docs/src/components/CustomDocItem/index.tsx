import React, { useEffect } from 'react';
import ActualDocItem from '@theme/DocItem';
import HasuraConBanner from '@site/src/components/HasuraConBanner';
import GraphQLWithHasuraBanner from '@site/src/components/GraphQLWithHasuraBanner';
import CustomFooter from '@site/src/components/CustomFooter';
import styles from './styles.module.scss';
import { Redirect } from '@docusaurus/router';

const CustomDocItem = props => {
  useEffect(() => {
    // This function is adds <wbr> tags to code blocks within a table
    // wherever an _ or . is present. We do this since so many environment
    // variables are incredibly long and the word breaks are not happening
    // in a user-friendly way.
    const tables = document.querySelectorAll('table');
    // code blocks inside of tables
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
    // not code blocks, like the metadata-api request type page
    const metadataApiTable = document.querySelector('.api-metadata-request-type-table');
    if (metadataApiTable) {
      const cells = metadataApiTable.querySelectorAll('td');
      cells.forEach(cell => {
        cell.innerHTML = cell.innerHTML.replace(/_/g, '_<wbr>');
      });
    }

    // dynamically updating the pg prop for the getting started cta
    function updateGettingStartedParam() {
      const linkElement = document.querySelector('.navbar__link.nav-link_getting-started');

      if (linkElement) {
        let page = props.location.pathname;
        page = page.slice(0, -1);
        page = page.replace('/docs/', 'docs_v2_').replace('latest/', '').replace(/\//g, '_');

        const href = linkElement.getAttribute('href');
        const newHref = href.replace(/pg=([^&]+)/, `pg=${page}`);
        linkElement.setAttribute('href', newHref);
      }
    }

    updateGettingStartedParam();
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
        <HasuraConBanner {...props} />
        <GraphQLWithHasuraBanner />
        <CustomFooter />
      </div>
    </div>
  );
};

export default CustomDocItem;
