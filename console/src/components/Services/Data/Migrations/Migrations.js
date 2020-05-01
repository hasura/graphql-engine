import React from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import Toggle from '../../../Common/Toggle/Toggle';
import { updateMigrationModeStatus } from '../../../Main/Actions';
import { getConfirmation } from '../../../Common/utils/jsUtils';

const Migrations = ({ dispatch, migrationMode }) => {
  const styles = require('./Migrations.scss');

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
          <a
            href="https://hasura.io/docs/1.0/graphql/manual/migrations/index.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            Hasura migrations guide
          </a>
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
        <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
          Database Migrations
        </h2>
        <div className="clearfix" />
      </div>
      <div className={styles.add_mar_top}>
        <div className={`${styles.padd_left_remove} col-xs-8`}>
          {getNotesSection()}
        </div>
        <div className="clearfix" />
        <div className={styles.migration_mode + ' ' + styles.add_mar_top}>
          <label>
            <span> Allow Postgres schema changes via console </span>
            <Toggle
              checked={migrationMode}
              icons={false}
              onChange={handleMigrationModeToggle}
            />
          </label>
        </div>
      </div>
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
