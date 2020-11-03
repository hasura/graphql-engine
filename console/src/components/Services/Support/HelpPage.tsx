import React from 'react';

import styles from './Support.scss';
import discord from './images/discord.svg';
import docs from './images/docs.svg';
import stackOverflow from './images/stack-overflow.svg';
import github from './images/github.svg';

const CHECK_FORUMS = `
If you need any help with developing on Hasura, you can check out
these various Hasura forums. Our community members include some very
experienced engineers from some of the world’s most exciting
companies, and many of them have been using Hasura in production for a
long time.`;

const supportListState = [
  {
    brand: discord,
    title: 'Discord',
    description:
      'Our community hangs out here. Join discord to ask/help folks in the community.',
    link: 'https://discord.com/invite/hasura',
  },
  {
    brand: docs,
    title: 'Docs',
    description: 'Head to docs to search for what you’re looking for.',
    link: 'https://hasura.io/docs/',
  },
  {
    brand: stackOverflow,
    title: 'StackOverflow',
    description: 'Ask your Hasura questions here and tag as ‘hasura’',
    link: 'https://stackoverflow.com/questions/tagged/hasura',
  },
  {
    brand: github,
    title: 'GitHub',
    description:
      'Create an issue on GitHub to report bugs, suggest improvements or give us a star!',
    link: 'https://github.com/hasura/graphql-engine/',
  },
];

const HelpPage = () => {
  return (
    <div
      className={`${styles.padd_left_remove} ${styles.supportForumWrapper} container-fluid ${styles.padd_top}`}
    >
      <div className={styles.padd_left}>
        <h2 className={`${styles.headerText} ${styles.inline_block}`}>
          Support Forums
        </h2>
        <div className={`${styles.descriptionText} ${styles.wd60}`}>
          {CHECK_FORUMS}
        </div>
        <div className={styles.supportWrapper}>
          {supportListState.map((list, index) => {
            return (
              <div
                className={`col-md-6 col-sm-6 col-xs-12 ${styles.padd_remove} ${styles.supportDisplay}`}
              >
                <a
                  key={index}
                  href={list.link}
                  target="_blank"
                  rel="noopener noreferrer"
                  className={styles.supportFlex}
                >
                  <div className={styles.supportList}>
                    <div className={styles.supportBrand}>
                      <img src={list.brand} alt={list.title} />
                    </div>
                    <div className={styles.supportContainer}>
                      <div className={styles.title}>{list.title}</div>
                      <div className={styles.descriptionText}>
                        {list.description}
                      </div>
                    </div>
                  </div>
                </a>
              </div>
            );
          })}
        </div>
        <div className={`${styles.descriptionText} ${styles.wd60}`}>
          If you would like to talk to our Product Specialists, head to our{' '}
          <a
            href="https://hasura.io/help"
            // eslint-disable-next-line react/jsx-no-target-blank
            target="_blank"
          >
            help page
          </a>{' '}
          to chat with us or{' '}
          <a
            href="https://calendly.com/hasura/prod-expert-call"
            // eslint-disable-next-line react/jsx-no-target-blank
            target="_blank"
          >
            set up a call
          </a>
          .
        </div>
      </div>
    </div>
  );
};
export default HelpPage;
