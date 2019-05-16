import React from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
import 'brace/mode/sql';

import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import Toggle from 'react-toggle';
import '../../../Common/ReactToggle/ReactToggleOverrides.css';

import { updateMigrationModeStatus } from '../../../Main/Actions';

const MigrationsHome = ({ dispatch, migrationMode }) => {
  const styles = require('./Migrations.scss');

  const handleMigrationModeToggle = () => {
    const isConfirm = window.confirm('Are you sure?');
    if (isConfirm) {
      dispatch(updateMigrationModeStatus());
    }
  };

  const migrationTip = (
    <Tooltip id="tooltip-migration">
      Modifications to the underlying postgres schema should be tracked as
      migrations.
    </Tooltip>
  );

  return (
    <div className={'container-fluid'}>
      <Helmet title="Migrations - Data | Hasura" />
      <div className={styles.add_mar_top}>
        <OverlayTrigger placement="right" overlay={migrationTip}>
          <i className={'fa fa-info-circle'} aria-hidden="true" />
        </OverlayTrigger>
        <div className={styles.migration_mode}>
          <label>
            <span> Allow postgres schema changes </span>
            <Toggle
              checked={migrationMode}
              icons={false}
              onChange={handleMigrationModeToggle}
            />
          </label>
        </div>
      </div>
      <div className={styles.add_mar_top}>
        <b>Note</b>
        <ul className={styles.ul_left_small + ' ' + styles.add_mar_top_small}>
          <li>
            Recommend that you turn this off if you're working with an existing
            app or database.
          </li>
        </ul>
      </div>
      <hr />
    </div>
  );
};

MigrationsHome.propTypes = {
  dispatch: PropTypes.func.isRequired,
  migrationMode: PropTypes.bool.isRequired,
};

const mapStateToProps = state => ({
  ...state.rawSQL,
  migrationMode: state.main.migrationMode,
});

const migrationsConnector = connect => connect(mapStateToProps)(MigrationsHome);

export default migrationsConnector;
