import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import styles from './index.module.scss';
import hasuras from '../../static/img/hasuras.png';
import VersionedLink from '../components/VersionedLink';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero', styles.heroBanner)}>
      <div className="container">
        <h1 className="hero__title">{siteConfig.title}</h1>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.links}>
          <VersionedLink
            className="button button--primary button--lg"
            to="/graphql/core/index">
            Hasura Core Docs
          </VersionedLink>
          <VersionedLink
            className="button button--success button--lg"
            to="/graphql/cloud/index">
            Hasura Cloud Docs
          </VersionedLink>
        </div>
        <div className={styles.links}>
          <Link
            className="button button--secondary button--lg"
            to="/wiki/page-structure">
            Docs Page Structure
          </Link>
          <Link
            className="button button--warning button--lg"
            to="/wiki/category/rst-vs-mdx-guide/">
            RST vs MDX Guide
          </Link>
        </div>
        <img src={hasuras} alt="Hasuras Image" />
      </div>
    </header>
  );
}

export default function Home(): JSX.Element {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title={siteConfig.title}
      description="Hasura gives you instant GraphQL APIs on your data sources. Point Hasura to your preferred internal and external data sources, setup relationships and security rules on your data models across sources and get a managed unified GraphQL API to build modern applications, instantly.">
      <HomepageHeader />
      <main>
      </main>
    </Layout>
  );
}
