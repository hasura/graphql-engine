import React from 'react';
import styles from './styles.module.scss';
import Beaker from '@site/static/icons/beaker.svg';

const isDependent = `This quickstart/recipe is dependent upon the docs e-commerce sample app. If you haven't already deployed the sample app, you can do so with one click below. If you've already deployed the sample app, simply use <a href="https://cloud.hasura.io" target="_blank" rel="noopener noreferrer">your existing project.</a>`;
const isStandalone = `You can use this quickstart with any project, but it pairs well with our docs e-commerce sample app, which you can deploy to Hasura Cloud with one click below. If you've already deployed the sample app, <a href="https://cloud.hasura.io" target="_blank" rel="noopener noreferrer">access your existing project.</a>`;

function createMarkup(dependent) {
  return { __html: dependent ? isDependent : isStandalone };
}

const SampleAppBlock = ({ dependent }) => (
  <div className="theme-admonition theme-admonition-info alert alert--info admonition_node_modules-@docusaurus-theme-classic-lib-theme-Admonition-styles-module">
    <div className={styles.heading}>
      <div className={styles.beaker}>
        <Beaker />
      </div>
      <h5 className="admonition-title">DOCS E-COMMERCE SAMPLE APP</h5>
    </div>
    <p dangerouslySetInnerHTML={createMarkup(dependent)} />
    <a
      href="https://cloud.hasura.io/deploy?github_repo=https://github.com/hasura/docs-sample-app&hasura_dir=/"
      target={'_blank'}
      rel={'noopener'}
    >
      <img
        src="https://hasura.io/deploy-button.svg"
        alt="Deploy to Hasura Cloud"
      />
    </a>
  </div>
);

export default SampleAppBlock;
