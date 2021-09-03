import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';
import styles from '../Actions.scss';

// import TryItOut from '../../Common/Landing/TryItOut';

class Landing extends React.Component {
  render() {
    const { readOnlyMode } = this.props;

    const { dispatch } = this.props;
    const getIntroSection = () => {
      return (
        <div>
          <TopicDescription
            title="What are Actions?"
            imgUrl={`${globals.assetsPath}/common/img/actions.png`}
            imgAlt="Actions"
            description="Actions are custom queries or mutations that are resolved via HTTP handlers. Actions can be used to carry out complex data validations, data enrichment from external sources or execute just about any custom business logic."
            knowMoreHref="https://hasura.io/docs/latest/graphql/core/actions/index.html"
          />
          <hr className={`${styles.clear_fix} my-lg`} />
        </div>
      );
    };

    const getAddBtn = () => {
      const handleClick = e => {
        e.preventDefault();
        dispatch(push(`${globals.urlPrefix}${appPrefix}/manage/add`));
      };

      const addBtn = !readOnlyMode && (
        <Button
          data-test="data-create-actions"
          color="yellow"
          size="sm"
          className={styles.add_mar_left}
          onClick={handleClick}
        >
          Create
        </Button>
      );

      return addBtn;
    };

    return (
      <div
        className={`${styles.padd_left_remove} ${styles.actionsWrapper} container-fluid ${styles.padd_top}`}
      >
        <div className={styles.padd_left}>
          <Helmet title={`${pageTitle} | Hasura`} />
          <div>
            <div className={styles.display_flex}>
              <h2 className={`${styles.headerText} ${styles.inline_block}`}>
                Actions
              </h2>
              {getAddBtn()}
            </div>
            <hr className="my-md" />

            {getIntroSection()}
          </div>
        </div>
      </div>
    );
  }
}

export default Landing;
