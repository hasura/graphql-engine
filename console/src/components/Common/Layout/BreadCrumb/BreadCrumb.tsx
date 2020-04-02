import React from 'react';
import { Link } from 'react-router';

export type BreadCrumbItem = {
  url: string;
  title: string;
};

const BreadCrumb = ({ breadCrumbs }: { breadCrumbs: BreadCrumbItem[] }) => {
  const styles = require('../../TableCommon/Table.scss');

  let bC = null;

  if (breadCrumbs && breadCrumbs.length > 0) {
    bC = breadCrumbs.map((b: BreadCrumbItem, i: number) => {
      let bCElem;

      const addArrow = () => (
        <React.Fragment>
          &nbsp;
          <i key={'l' + i} className="fa fa-angle-right" aria-hidden="true" />
          &nbsp;
        </React.Fragment>
      );

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

export default BreadCrumb;
