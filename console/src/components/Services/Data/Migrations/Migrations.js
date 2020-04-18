import React from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import Toggle from 'react-toggle';
import 'react-toggle/style.css';

import { updateMigrationModeStatus } from '../../../Main/Actions';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { Heading, TextLink, Box, Text, Flex } from '../../../UIKit/atoms';
import '../../../Common/ReactToggle/ReactToggleOverrides.css';
import styles from './Migrations.scss';

const Migrations = ({ dispatch, migrationMode }) => {
  const handleMigrationModeToggle = () => {
    const isOk = getConfirmation();
    if (isOk) {
      dispatch(updateMigrationModeStatus());
    }
  };

  const getNotesSection = () => {
    return (
      <ul>
        <li>Migrations are used to track changes to the database schema.</li>
        <li>
          If you are managing database migrations externally, it is recommended
          that you disable making schema changes via the console.
        </li>
        <li>
          Read more about managing migrations with Hasura at the{' '}
          <TextLink
            href="https://hasura.io/docs/1.0/graphql/manual/migrations/index.html"
            target="_blank"
            hover="underline"
          >
            Hasura migrations guide
          </TextLink>
        </li>
      </ul>
    );
  };

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top}`}
    >
      <Helmet title="Migrations - Data | Hasura" />
      <div className={styles.subHeader}>
        <Heading as="h2" pb="0px" fontSize="18px">
          Database Migrations
        </Heading>
        <div className="clearfix" />
      </div>
      <Box mt="20px">
        <div className={`${styles.padd_left_remove} col-xs-8`}>
          {getNotesSection()}
        </div>
        <div className="clearfix" />
        <Box mt="20px" display="inline-block" ml="10px">
          <Flex as="label">
            <Text fontWeight="bold" ml="10px" display="inline-block">
              Allow Postgres schema changes via console
            </Text>
            <Toggle
              checked={migrationMode}
              icons={false}
              onChange={handleMigrationModeToggle}
            />
          </Flex>
        </Box>
      </Box>
    </div>
  );
};

Migrations.propTypes = {
  dispatch: PropTypes.func.isRequired,
  migrationMode: PropTypes.bool.isRequired,
};

const mapStateToProps = state => ({
  ...state.rawSQL,
  migrationMode: state.main.migrationMode,
});

const migrationsConnector = connect => connect(mapStateToProps)(Migrations);

export default migrationsConnector;
