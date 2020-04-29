import React from 'react';

import Common from '../Common/Common';
import { addRemoteSchema, RESET } from './addRemoteSchemaReducer';
import Helmet from 'react-helmet';
import Button from '../../../Common/Button/Button';
import { pageTitle } from '../constants';
import { Heading } from '../../../UIKit/atoms';
import styles from '../RemoteSchema.scss';

class Add extends React.Component {
  componentWillUnmount() {
    this.props.dispatch({ type: RESET });
  }

  render() {
    const { isRequesting, dispatch } = this.props;

    return (
      <div className={styles.addWrapper}>
        <Helmet title={`Add ${pageTitle} - ${pageTitle}s | Hasura`} />
        <Heading as="h2" fontSize="18px" pb="20px">
          Add a new remote schema
        </Heading>
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

export default connect => connect(mapStateToProps)(Add);
