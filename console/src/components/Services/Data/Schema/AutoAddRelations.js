import PropTypes from 'prop-types';

import React, { Component } from 'react';
import {
  autoTrackRelations,
  autoAddRelName,
} from '../TableRelationships/Actions';
import { getRelDef } from '../TableRelationships/Relationships';
import Button from '../../../Common/Button/Button';

class AutoAddRelations extends Component {
  trackAllRelations = untrackedData => {
    this.props.dispatch(autoTrackRelations(untrackedData));
  };

  render() {
    const styles = require('../../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');

    const { untrackedRelations, dispatch } = this.props;

    if (untrackedRelations.length === 0) {
      return (
        <div
          className={styles.display_inline + ' ' + styles.padd_bottom}
          key="no-untracked-rel"
        >
          There are no untracked relations
        </div>
      );
    }

    const untrackData = untrackedRelations.map((obj, i) => {
      const objData = obj.data;

      const addRel = () => {
        dispatch(autoAddRelName(obj));
      };

      const handleAddRel = e => {
        e.preventDefault();
        addRel();
      };

      return (
        <div
          className={styles.padd_top_medium}
          key={`${objData.tableName}-${objData.rTable}-${i}`}
        >
          <Button
            className={styles.display_inline}
            size="xs"
            color="white"
            onClick={handleAddRel}
          >
            Add
          </Button>
          <div className={styles.display_inline + ' ' + styles.add_mar_left}>
            <span>
              <b>{objData.tableName}</b> &rarr; <b>{objData.rTable}</b>
            </span>
            &nbsp;&nbsp; - &nbsp;&nbsp;
            <span>
              {getRelDef(
                objData.isObjRel,
                objData.lcol,
                objData.rcol,
                objData.rTable
              )}
            </span>
          </div>
        </div>
      );
    });

    return (
      <div>
        {untrackedRelations.length === 0 ? (
          <div
            className={styles.display_inline + ' ' + styles.padd_bottom}
            key="no-untracked-rel"
          >
            There are no untracked relations
          </div>
        ) : (
          <div
            className={styles.display_inline + ' ' + styles.padd_bottom}
            key="untracked-rel"
          >
            There are {untrackedRelations.length} untracked relations
          </div>
        )}
        <Button
          onClick={this.trackAllRelations.bind(this, untrackedRelations)}
          className={`${styles.display_inline} ${styles.add_mar_left}`}
          color="white"
          size="xs"
          data-test="track-all-relationships"
        >
          Track All Relations
        </Button>
        <div className={styles.padd_top_small}>{untrackData}</div>
      </div>
    );
  }
}

AutoAddRelations.propTypes = {
  untrackedRelations: PropTypes.array.isRequired,
  schema: PropTypes.array.isRequired,
  dispatch: PropTypes.func.isRequired,
};

export default AutoAddRelations;
