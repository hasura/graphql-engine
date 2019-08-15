import React from 'react';
import PropTypes from 'prop-types';
import { Link } from 'react-router';

class BreadCrumb extends React.Component {
  render() {
    const { breadCrumbs } = this.props;
    const styles = require('../../TableCommon/Table.scss');

    let bC = null;

    if (breadCrumbs && breadCrumbs.length > 0) {
      bC = breadCrumbs.map((b, i) => {
        let bCElem;

        const Sp = () => {
          const space = ' ';
          return space;
        };

        const addArrow = () => [
          <Sp key={'breadcrumb-space-before' + i} />,
          <i key={'l' + i} className="fa fa-angle-right" aria-hidden="true" />,
          <Sp key={'breadcrumb-space-after' + i} />,
        ];

        const isLastElem = i === breadCrumbs.length - 1;

        if (!isLastElem) {
          bCElem = [
            <Link key={'l' + i} to={`${b.url}`}>
              {b.title}
            </Link>,
            addArrow(),
          ];
        } else {
          bCElem = [b.title];
        }

        return bCElem;
      });
    }

    return <div className={styles.dataBreadCrumb}>You are here: {bC}</div>;
  }
}

BreadCrumb.propTypes = {
  breadCrumbs: PropTypes.array.isRequired,
};

export default BreadCrumb;
