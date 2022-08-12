import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';

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
          <hr className="mt-lg mb-lg" />
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
          className="ml-5"
          onClick={handleClick}
        >
          Create
        </Button>
      );

      return addBtn;
    };

    return (
      <div>
        <div className="p-5">
          <Helmet title={`${pageTitle} | Hasura`} />
          <div>
            <div className={'flex'}>
              <h2 className="font-bold text-3xl pr-3">Actions</h2>
              {getAddBtn()}
            </div>
            <hr className="mt-5 mb-5" />
            {getIntroSection()}
          </div>
        </div>
      </div>
    );
  }
}

export default Landing;
