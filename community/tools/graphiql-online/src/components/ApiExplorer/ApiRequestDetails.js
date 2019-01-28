import React, { Component } from 'react';
import PropTypes from 'prop-types';

class ApiRequestDetails extends Component {
  render() {
    const styles = require('./ApiExplorer.scss');
    return (
      <div className={styles.apiRequestWrapper + ' ' + styles.apiContentPadd}>
        <div className={styles.apiRequestContent}>{this.props.description}</div>
        <div className={styles.built}>
          Built with <i className="fa fa-heart" /> by <a href={'http://hasura.io/'} target={'_blank'}>Hasura</a>
        </div>
        <div className={styles.apiHasura}>
          <a href="https://github.com/hasura/graphql-engine/tree/master/community/tools/graphiql-online" target={'_blank'}>
            <i className="fa fa-github" />
          </a>
        </div>
      </div>
    );
  }
}

ApiRequestDetails.propTypes = {
  title: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
};

export default ApiRequestDetails;
