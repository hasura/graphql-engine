import React from 'react';
import Common from '../Common/Common';

import { addRemoteSchema, RESET } from './addRemoteSchemaReducer';
import Helmet from 'react-helmet';
import Button from '../../../Common/Button/Button';

import { pageTitle } from '../constants';

import styles from '../RemoteSchema.scss';
import { defaultHeader } from '../../../Common/Headers/Headers';

class Add extends React.Component {
  state = {
    headers: [{...defaultHeader}],
  };
  componentWillUnmount() {
    this.props.dispatch({ type: RESET });
  }
  setHeaders = headers => this.setState({ headers });
  onSubmit = e => {
    e.preventDefault();
    const { headers } = this.state;
    this.props.dispatch(addRemoteSchema(headers));
  };

  render() {
    const { isRequesting } = this.props;

    return (
      <div className={styles.addWrapper}>
        <Helmet title={`Add ${pageTitle} - ${pageTitle}s | Hasura`} />
        <div className={styles.heading_text}>Add a new remote schema</div>
        <form onSubmit={this.onSubmit}>
          <Common
            {...this.props}
            headers={this.state.headers}
            setHeaders={this.setHeaders}
          />
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
  };
};

export default connect => connect(mapStateToProps)(Add);
