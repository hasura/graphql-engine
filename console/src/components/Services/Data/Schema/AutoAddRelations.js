/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import {
  autoTrackRelations,
  autoAddRelName,
} from '../TableRelationships/Actions';
import { getRelationshipLine } from '../TableRelationships/Relationships';
import suggestedRelationshipsRaw from '../TableRelationships/autoRelations';

class AutoAddRelations extends Component {
  trackAllRelations = () => {
    this.props.dispatch(autoTrackRelations());
  };
  render() {
    const { schema, untrackedRelations, dispatch } = this.props;
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
    const untrackedIndivHtml = [];
    schema.map(table => {
      const currentTable = table.table_name;
      const currentTableRel = suggestedRelationshipsRaw(currentTable, schema);
      currentTableRel.objectRel.map(obj => {
        untrackedIndivHtml.push(
          <div
            className={styles.padd_top_medium}
            key={'untrackedIndiv' + table.table_name}
          >
            <button
              className={`${styles.display_inline} btn btn-xs btn-default`}
              onClick={e => {
                e.preventDefault();
                handleAutoAddIndivRel(obj);
              }}
            >
              Add
            </button>
            <div className={styles.display_inline + ' ' + styles.add_pad_left}>
              <b>{obj.tableName}</b> -{' '}
              {getRelationshipLine(
                obj.isObjRel,
                obj.lcol,
                obj.rcol,
                obj.rTable
              )}
            </div>
          </div>
        );
      });
      currentTableRel.arrayRel.map(obj => {
        untrackedIndivHtml.push(
          <div
            className={styles.padd_top_medium}
            key={'untrackedIndiv' + table.table_name}
          >
            <button
              className={`${styles.display_inline} btn btn-xs btn-default`}
              onClick={e => {
                e.preventDefault();
                handleAutoAddIndivRel(obj);
              }}
            >
              Add
            </button>
            <div className={styles.display_inline + ' ' + styles.add_pad_left}>
              <b>{obj.tableName}</b> -{' '}
              {getRelationshipLine(
                obj.isObjRel,
                obj.lcol,
                obj.rcol,
                obj.rTable
              )}
            </div>
          </div>
        );
      });
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
        <button
          onClick={this.trackAllRelations}
          className={
            styles.display_inline +
            ' btn btn-xs btn-default ' +
            styles.add_mar_left
          }
          data-test="track-all-relationships"
        >
          Track All Relations
        </button>
        <div className={styles.padd_top_small}>{untrackedIndivHtml}</div>
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
