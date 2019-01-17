/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import {
  autoTrackRelations,
  autoAddRelName,
} from '../TableRelationships/Actions';
import { getRelationshipLine } from '../TableRelationships/Relationships';
import Button from '../../Layout/Button/Button';

class AutoAddRelations extends Component {
  trackAllRelations = untrackedData => {
    this.props.dispatch(autoTrackRelations(untrackedData));
  };
  render() {
    const { untrackedRelations, dispatch } = this.props;
    const styles = require('../PageContainer/PageContainer.scss');
    const handleAutoAddIndivRel = obj => {
      dispatch(autoAddRelName(obj));
    };

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
    const untrackData = untrackedRelations.map(obj => {
      return (
        <div
          className={styles.padd_top_medium}
          key={'untrackedIndiv' + obj.data.tableName}
        >
          <Button
            className={`${styles.display_inline}`}
            size="xs"
            color="white"
            onClick={e => {
              e.preventDefault();
              handleAutoAddIndivRel(obj);
            }}
          >
            Add
          </Button>
          <div className={styles.display_inline + ' ' + styles.add_pad_left}>
            <b>{obj.data.tableName}</b> -{' '}
            {getRelationshipLine(
              obj.data.isObjRel,
              obj.data.lcol,
              obj.data.rcol,
              obj.data.rTable
            )}
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
