import React from 'react';
import Common from '../Common/Common';

import { addRemoteSchema, RESET } from './addRemoteSchemaReducer';
import Helmet from 'react-helmet';
import Button from '../../../Common/Button/Button';

import { pageTitle } from '../constants';
import { connect } from 'react-redux';

class Add extends React.Component {
  componentWillUnmount() {
    this.props.dispatch({ type: RESET });
  }

  render() {
    const styles = require('../RemoteSchema.scss');

    const { isRequesting, dispatch } = this.props;

    return (
      <div className={styles.addWrapper}>
        <Helmet title={`Add ${pageTitle} - ${pageTitle}s | Hasura`} />
        <div className={styles.heading_text}>Add a new remote schema</div>
        <form
          onSubmit={e => {
            e.preventDefault();
            dispatch(addRemoteSchema());
          }}
        >
          <Common {...this.props} />
          <div className={styles.commonBtn}>
            <Button
              type="submit"
              color="yellow"
              size="sm"
              disabled={isRequesting}
              data-test="add-remote-schema-submit"
            >
              {isRequesting ? 'Adding...' : 'Add Remote Schema'}
            </Button>
            {/*
            <button className={styles.default_button}>Cancel</button>
            */}
          </div>
        </form>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    ...state.remoteSchemas.addData,
    ...state.remoteSchemas.headerData,
  };
};

const ConnectedAdd = connect(mapStateToProps)(Add);
export default ConnectedAdd;
