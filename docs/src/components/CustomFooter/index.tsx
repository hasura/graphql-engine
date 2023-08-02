import React, { useEffect, useState } from 'react';
import ReactDOM from 'react-dom';
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import ContributionPointer from './ContributionPointer';
import GithubIcon from '@site/static/icons/github.svg';
import TwitterIcon from '@site/static/icons/twitter.svg';
import DiscordIcon from '@site/static/icons/discord.svg';
import LinkedInIcon from '@site/static/icons/linkedin.svg';
import YoutubeIcon from '@site/static/icons/youtube.svg';
import styles from './styles.module.scss';
import { v4 as uuidv4 } from 'uuid';
const CustomFooter = () => {
  const [element, setElement] = useState(null);
  useEffect(() => {
    if (!localStorage.getItem('hasuraDocsUserID')) {
      const userID = uuidv4();
      localStorage.setItem('hasuraDocsUserID', userID);
    }
  }, []);

  useEffect(() => {
    const editMetaRow = document.querySelector('.theme-doc-footer-edit-meta-row > .col');
    setElement(editMetaRow);
    editMetaRow.style.position = 'relative';
    const div = document.createElement('div');
    editMetaRow.appendChild(div);
    ReactDOM.render(<ContributionPointer element={element} />, div);
  }, [element]);

  return (
    <footer className={styles['custom-footer-wrapper']}>
      <div className={styles['logo-wrapper']}>
        <img src={useBaseUrl('/img/logo-light.svg')} className={styles['dark-theme-logo']} />
        <img src={useBaseUrl('/img/logo.svg')} className={styles['light-theme-logo']} />
      </div>
      <div className={styles['copyright']}>{`© ${new Date().getFullYear()} Hasura Inc. All rights reserved`}</div>
      <div className={styles['footerSocialIconsWrapper']}>
        <div className={styles['socialBrands']}>
          <Link href={'https://github.com/hasura/graphql-engine'} rel="noopener noreferrer" aria-label={'Github'}>
            <GithubIcon />
          </Link>
        </div>
        <div className={styles['socialBrands']}>
          <Link href={'https://twitter.com/hasurahq'} rel="noopener noreferrer" aria-label={'Twitter'}>
            <TwitterIcon />
          </Link>
        </div>
        <div className={styles['socialBrands']}>
          <Link href={'https://discord.com/invite/hasura'} rel="noopener noreferrer" aria-label={'Discord'}>
            <DiscordIcon />
          </Link>
        </div>
        <div className={styles['socialBrands']}>
          <Link
            href={'https://www.youtube.com/channel/UCZo1ciR8pZvdD3Wxp9aSNhQ'}
            rel="noopener noreferrer"
            aria-label={'Youtube'}
          >
            <YoutubeIcon />
          </Link>
        </div>
        <div className={styles['socialBrands']}>
          <Link href={'https://www.linkedin.com/company/hasura'} rel="noopener noreferrer" aria-label={'Linkedin'}>
            <LinkedInIcon />
          </Link>
        </div>
      </div>
    </footer>
  );
};

export default CustomFooter;
