import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';
import { Heading, Flex, Box } from '../../../UIKit/atoms';
import styles from '../Actions.scss';
// import TryItOut from '../../Common/Landing/TryItOut';

const actionsArchDiagram = `${globals.assetsPath}/common/img/actions.png`;

const Landing = ({ dispatch }) => {
  const getIntroSection = () => {
    return (
      <div>
        <TopicDescription
          title="What are Actions?"
          // imgUrl={`${globals.assetsPath}/common/img/remote_schema.png`} // TODO: update image & description
          imgUrl={actionsArchDiagram}
          imgAlt="Actions"
          description="Actions are custom mutations that are resolved via HTTP handlers. Actions can be used to carry out complex data validations, data enrichment from external sources or execute just about any custom business logic."
        />
        <hr className={styles.clear_fix} />
      </div>
    );
  };

  const getAddBtn = () => {
    const handleClick = e => {
      e.preventDefault();
      dispatch(push(`${globals.urlPrefix}${appPrefix}/manage/add`));
    };

    const addBtn = (
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
    <Box pl="0px" pt="20px" className="container-fluid">
      <div className={styles.padd_left}>
        <Helmet title={`${pageTitle} | Hasura`} />
        <Flex>
          <Heading as="h2" fontSize="h2" display="inline-block" pr="20px">
            Actions
          </Heading>
          {getAddBtn()}
        </Flex>
        <hr />

        {getIntroSection()}
      </div>
    </Box>
  );
};

export default Landing;
