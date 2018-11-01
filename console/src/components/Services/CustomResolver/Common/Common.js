import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

const graphqlurl = (
  <Tooltip id="tooltip-cascade">
    Remote GraphQL server’s URL. E.g. https://my-domai/v1alpha1/graphql
  </Tooltip>
);
const header = (
  <Tooltip id="tooltip-cascade">
    Configure headers for requests to remote GraphQL server. All incoming
    request headers will be forwarded
  </Tooltip>
);
const schema = (
  <Tooltip id="tooltip-cascade">
    Give this GraphQL schema a friendly name.
  </Tooltip>
);

class Common extends React.Component {
  render() {
    const styles = require('../Styles.scss');
    return (
      <div className={styles.CommonWrapper}>
        <div className={styles.subheading_text}>
          Remote GraphQL server URL *
          <OverlayTrigger placement="right" overlay={graphqlurl}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <div className={styles.addPaddCommom}>
          <label className={styles.radioLabel + ' radio-inline col-md-3'}>
            <input type="radio" value="" />
            Enter manually:
          </label>
          <label
            className={
              styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
            }
          >
            <input
              className={'form-control'}
              type="text"
              placeholder="GraphQL server URL"
            />
          </label>
        </div>
        <div className={styles.addPaddCommom}>
          <label className={styles.radioLabel + ' radio-inline col-md-3'}>
            <input type="radio" value="" />
            Pick from environment variable:
          </label>
          <label
            className={
              styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
            }
          >
            <input
              className={'form-control'}
              type="text"
              placeholder="env_variable_name"
            />
          </label>
        </div>
        <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
          Header *
          <OverlayTrigger placement="right" overlay={header}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <input
            type="text"
            className={styles.input + ' form-control ' + styles.add_mar_right}
          />
          <select
            className={
              'form-control ' + styles.add_pad_left + ' ' + styles.add_mar_right
            }
          >
            <option disabled value="-- value type --">
              -- value type --
            </option>
          </select>
          <input
            type="text"
            className={
              styles.inputDefault +
              ' form-control ' +
              styles.defaultWidth +
              ' ' +
              styles.add_pad_left
            }
            placeholder="value"
          />
        </div>
        <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
          Schema alias *
          <OverlayTrigger placement="right" overlay={schema}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <label
          className={
            styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
          }
        >
          <input
            className={'form-control'}
            type="text"
            placeholder="My-graphql-schema"
          />
        </label>
      </div>
    );
  }
}

export default Common;
