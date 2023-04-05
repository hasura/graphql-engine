import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import { Button } from '../../../../new-components/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';
import { FaEdit, FaFileImport } from 'react-icons/fa';
import { Badge } from '../../../../new-components/Badge';

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
            learnMoreHref="https://hasura.io/docs/latest/graphql/core/actions/index.html"
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
        <div className="ml-md">
          <Button
            icon={<FaEdit />}
            data-testid="data-create-actions"
            mode="primary"
            onClick={handleClick}
          >
            Create
          </Button>
        </div>
      );

      return addBtn;
    };

    return (
      <Analytics name="Actions" {...REDACT_EVERYTHING}>
        <div>
          <div className="p-5 bootstrap-jail">
            <Helmet title={`${pageTitle} | Hasura`} />
            <div>
              <div className={'flex'}>
                <h2 className="font-bold text-3xl pr-3">Actions</h2>
                {getAddBtn()}
                <Analytics
                  name="action-tab-btn-import-action-from-openapi"
                  passHtmlAttributesToChildren
                >
                  <Button
                    icon={<FaFileImport />}
                    className="ml-2"
                    onClick={() => {
                      dispatch(
                        push(`${globals.urlPrefix}${appPrefix}/manage/add-oas`)
                      );
                    }}
                  >
                    Import from OpenAPI
                    <Badge className="ml-2 font-xs" color="purple">
                      New
                    </Badge>
                  </Button>
                </Analytics>
              </div>
              <hr className="mt-5 mb-5" />
              {getIntroSection()}
            </div>
          </div>
        </div>
      </Analytics>
    );
  }
}

export default Landing;
