import React from 'react';
import PropTypes from 'prop-types';
import { Link } from 'react-router';

import { Icon } from '../../../UIKit/atoms';
import styles from '../../TableCommon/Table.scss';

const BreadCrumb = ({ breadCrumbs }) => {
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
        <Icon key={'l' + i} type="right" />,

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
};

BreadCrumb.propTypes = {
  breadCrumbs: PropTypes.array.isRequired,
};

export default BreadCrumb;
