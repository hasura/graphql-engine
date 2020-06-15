import React from 'react';
const styles = require('./Support.scss');

const discord = require('./images/discord.svg');
const docs = require('./images/docs.svg');
const stackOverflow = require('./images/stack-overflow.svg');
const github = require('./images/github.svg');
const supportListState = [
  {
    brand: discord,
    title: 'Discord',
    description: 'Our community hangs out here. Join discord to ask/help folks in the community.',
    link: 'https://discord.com/invite/hasura',
  },
  {
    brand: docs,
    title: 'Docs',
    description: 'Head to docs to search for what you’re looking for & if you can’t find it, please do one of other steps above :)',
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
    description: 'Create an issue on GitHub to Report bugs, suggest improvements or give us a star!',
    link: 'https://github.com/hasura/graphql-engine/',
  },
]
const Support = () => {
  const supportList = supportListState.map((list, index)=> {
    return (
      <div className={'col-md-6 col-sm-6 col-xs-12 ' + styles.padd_remove + ' ' + styles.supportDisplay}>
        <a key={index}
          href={list.link} target='_blank' rel='noopener noreferrer'
          className={styles.supportFlex}
        >
          <div className={styles.supportList}>
            <div className={styles.supportBrand}>
              <img src={list.brand} alt={list.title} />
            </div>
            <div className={styles.supportContainer}>
              <div className={styles.title}>
                {list.title}
              </div>
              <div className={styles.descriptionText}>
                {list.description}
              </div>
            </div>
          </div>
        </a>
      </div>
    )
  })
  return (
    <div
      className={`${styles.padd_left_remove} ${styles.supportForumWrapper} container-fluid ${styles.padd_top}`}
    >
      <div className={styles.padd_left}>
        <h2 className={`${styles.headerText} ${styles.inline_block}`}>
          Support Forums
        </h2>
        <div className={styles.descriptionText + ' ' + styles.wd60}>
          If you need any help with developing on Hasura, you can check out these various Hasura forums. Our community members include some very experienced engineers from some of the world’s most exciting companies, and many of them have been using Hasura in Production for a long time.
        </div>
        <div className={styles.supportWrapper}>
          {supportList}
        </div>
        <div className={styles.descriptionText + ' ' + styles.wd60}>
          If you want to talk to our Product Specialists, please email us at <a href="mailto:support@hasura.io">support@hasura.io</a> or <a href='https://calendly.com/hasura/prod-expert-call' target='_blank'>Set up a call</a>.
        </div>
      </div>
    </div>
  )
}
export default Support
