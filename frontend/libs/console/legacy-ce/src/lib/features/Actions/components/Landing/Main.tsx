import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { pageTitle } from '../../constants';
import { Button, Badge, TopicDescription, Separator } from '@hasura/shared/ui';
import { FaEdit, FaFileImport } from 'react-icons/fa';
import { useNavigate } from 'react-router';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { Flex, Heading } from '@radix-ui/themes';
import { dataRoutes } from '@hasura/shared/utils';

const Landing = () => {
  useDocumentTitle(`${pageTitle} | Hasura`);

  const navigate = useNavigate();
  const { readOnlyMode, envVars } = useAppContext();

  const getIntroSection = () => {
    return (
      <div>
        <TopicDescription
          title="What are Actions?"
          imgUrl={`${envVars.assetsPath}/common/img/actions.png`}
          imgAlt="Actions"
          description="Actions are custom queries or mutations that are resolved via HTTP handlers. Actions can be used to carry out complex data validations, data enrichment from external sources or execute just about any custom business logic."
          learnMoreHref="https://hasura.io/docs/latest/graphql/core/actions/index.html"
        />
        <Separator className="my-6" />
      </div>
    );
  };

  const getAddBtn = () => {
    const handleClick = (e) => {
      e.preventDefault();
      navigate(dataRoutes.createAction);
    };

    const addBtn = !readOnlyMode && (
      <div className="ml-4">
        <Button
          leftIcon={FaEdit}
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
          <div>
            <Flex align="center" gap="2">
              <Heading size="5">Actions</Heading>
              {getAddBtn()}
              <Analytics
                name="action-tab-btn-import-action-from-openapi"
                passHtmlAttributesToChildren
              >
                <Button
                  mode="default"
                  leftIcon={FaFileImport}
                  onClick={() => {
                    navigate(dataRoutes.manageAction('add-oas'));
                  }}
                >
                  Import from OpenAPI
                  <Badge className="ml-2 font-xs" color="purple">
                    New
                  </Badge>
                </Button>
              </Analytics>
            </Flex>
            <Separator size="4" className="my-4" />
            {getIntroSection()}
          </div>
        </div>
      </div>
    </Analytics>
  );
};

export default Landing;
