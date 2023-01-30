import React, { useEffect, useState } from 'react';
import Layout from '@theme/Layout';
import Translate, { translate } from '@docusaurus/Translate';
import { PageMetadata } from '@docusaurus/theme-common';
import ThemedImage from '@theme/ThemedImage';
import { useLocation } from '@docusaurus/router';
import algoliasearch from 'algoliasearch';
import styles from './styles.module.scss';
import Light404 from '@site/static/img/light-404.png';
import Dark404 from '@site/static/img/dark-404.png';

export default function NotFound() {
  // State for handling search results
  const [searchResults, setSearchResults] = useState([]);
  const [loading, setLoading] = useState(true);

  // Algolia search initialization
  const client = algoliasearch('NS6GBGYACO', '8f0f11e3241b59574c5dd32af09acdc8');
  const index = client.initIndex('hasura-graphql');

  // Get the current location
  const location = useLocation();

  // useEffect to handle search
  useEffect(() => {
    // Get the search query from the URL
    const query = location.pathname;

    // No-no words
    const removeList = ['docs', 'latest', 'index'];

    // Remove the no-no words from the query
    const parsedQuery = query
      .split('/')
      .filter((word) => !removeList.includes(word))
      .join(' ');

    // Search
    index
      .search(parsedQuery, {
        hitsPerPage: 5,
      })
      .then(({ hits }) => {
        setSearchResults(hits);
        setLoading(false);
      })
      .catch((error) => {
        console.error(error);
      });
  }, []);

  return (
    <>
      <PageMetadata
        title={translate({
          id: 'theme.NotFound.title',
          message: 'Page Not Found',
        })}
      />
      <Layout>
        <main className='container margin-vert--xl'>
          <div className='row'>
            <div className={styles['content']}>
              <ThemedImage
                sources={{
                  light: Light404,
                  dark: Dark404,
                }}
                alt='404'
              />
              <div>
                <h1>
                  <Translate id='theme.NotFound.title' description='The title of the 404 page'>
                    We have a broken link or the URL entered doesn't exist in our docs.
                  </Translate>
                </h1>
                <p>
                  <Translate id='theme.NotFound.p2' description='The 2nd paragraph of the 404 page'>
                    {!loading && searchResults.length > 0
                      ? `Our team has been notified and they're on it. Is there a chance one of these links will help?`
                      : ``}
                  </Translate>
                </p>
                <ul className={styles['results']}>
                  {searchResults &&
                    searchResults.map((result) => (
                      <li key={result.objectID}>
                        <div>
                          <a href={result.url}>{result.hierarchy.lvl1}</a>
                        </div>
                      </li>
                    ))}
                </ul>
              </div>
            </div>
          </div>
        </main>
      </Layout>
    </>
  );
}
