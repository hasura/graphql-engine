import React from 'react';
import PropTypes from 'prop-types';
import { Link } from 'react-router';

class BreadCrumb extends React.Component {
  render() {
    const { breadCrumbs } = this.props;
    const styles = require('../../EventTrigger/TableCommon/Table.scss');
    const bC =
      breadCrumbs && breadCrumbs.length > 0
        ? breadCrumbs.map((b, i) => {
          const Sp = () => {
            const space = ' ';
            return space;
          };
          const addArrow = () => [
            <Sp key={'breadcrumb-space-before' + i} />,
            <i
              key={'l' + i}
              className="fa fa-angle-right"
              aria-hidden="true"
            />,
            <Sp key={'breadcrumb-space-after' + i} />,
          ];
          if (i !== breadCrumbs.length - 1) {
            return [
              <Link key={'l' + i} to={`${b.url}`}>
                {b.title}
              </Link>,
              addArrow(),
            ];
          }
          return [b.title];
        })
        : null;

    return <div className={styles.dataBreadCrumb}>You are here: {bC}</div>;
  }
}

BreadCrumb.propTypes = {
  breadCrumbs: PropTypes.array.isRequired,
};

export default BreadCrumb;
