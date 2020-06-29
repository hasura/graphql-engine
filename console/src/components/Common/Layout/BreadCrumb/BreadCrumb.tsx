import React from 'react';
import { Link } from 'react-router';
import styles from '../../TableCommon/Table.scss';

export type BreadCrumb = {
  url: string;
  title: string;
};

type Props = {
  breadCrumbs: BreadCrumb[];
};

const BreadCrumb: React.FC<Props> = ({ breadCrumbs }) => {
  let bC = null;

  if (breadCrumbs && breadCrumbs.length > 0) {
    bC = breadCrumbs.map((b: BreadCrumb, i: number) => {
      let bCElem;

      const addArrow = () => (
        <React.Fragment>
          &nbsp;
          <i
            key={`${b.title}-arrow`}
            className="fa fa-angle-right"
            aria-hidden="true"
          />
          &nbsp;
        </React.Fragment>
      );

      const isLastElem = i === breadCrumbs.length - 1;

      if (!isLastElem) {
        bCElem = [
          <Link key={`bc-title-${b.title}`} to={`${b.url}`}>
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
