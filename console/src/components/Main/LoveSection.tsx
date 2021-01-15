import React from 'react';
import styles from './Main.scss';
import PixelHeart from './images/components/PixelHeart';
import globals from '../../Globals';

type LoveSectionProps = {
  toggleLoveSection: () => void;
  closeLoveSection: () => void;
};

const LoveSection: React.FC<LoveSectionProps> = ({
  toggleLoveSection,
  closeLoveSection,
}) => (
  <>
    <div
      className={`${styles.pixelLoveSection} dropdown-toggle`}
      aria-expanded="false"
      onClick={toggleLoveSection}
    >
      <PixelHeart width={23} />
    </div>
    <ul key="main_love_2" className={`dropdown-menu ${styles.dropdown_menu}`}>
      <div className={styles.dropdown_menu_container}>
        <div className={styles.closeDropDown}>
          <i className="fa fa-close" onClick={closeLoveSection} />
        </div>
        <div className={styles.displayFlex}>
          <li className={styles.pixelText1}>
            Roses are red, <br />
            Violets are blue;
            <br />
            Star us on GitHub,
            <br />
            To make our <i className="fa fa-heart" /> go wooooo!
          </li>
          <li className="dropdown-item">
            <a
              href="https://github.com/hasura/graphql-engine"
              target="_blank"
              rel="noopener noreferrer"
            >
              <div className={styles.socialIcon}>
                <img
                  className="img img-responsive"
                  src={`${globals.assetsPath}/common/img/githubicon.png`}
                  alt="GitHub"
                />
              </div>
              <div className={styles.pixelText}>
                <i className="fa fa-star" />
                &nbsp; Star
              </div>
            </a>
          </li>
          <li className="dropdown-item">
            <a
              href="https://twitter.com/intent/tweet?hashtags=graphql,postgres&text=Just%20deployed%20a%20GraphQL%20backend%20with%20@HasuraHQ!%20%E2%9D%A4%EF%B8%8F%20%F0%9F%9A%80%0Ahttps://github.com/hasura/graphql-engine%0A"
              target="_blank"
              rel="noopener noreferrer"
            >
              <div className={styles.socialIcon}>
                <img
                  className="img img-responsive"
                  src={`${globals.assetsPath}/common/img/twittericon.png`}
                  alt="Twitter"
                />
              </div>
              <div className={styles.pixelText}>
                <i className="fa fa-twitter" />
                &nbsp; Tweet
              </div>
            </a>
          </li>
        </div>
      </div>
    </ul>
  </>
);

export default LoveSection;
