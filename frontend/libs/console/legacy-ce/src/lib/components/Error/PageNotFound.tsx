import React from 'react';
import { connect } from 'react-redux';

import { Link } from 'react-router';
import Helmet from 'react-helmet';
import globals from '../../Globals';

export class NotFoundError extends Error {}

type PageNotFoundProps = {
  resetCallback: () => void;
};

const PageNotFound = (props: PageNotFoundProps) => {
  const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

  const { resetCallback } = props;

  return (
    <div className="h-screen w-screen flex items-center justify-center ">
      <Helmet title="404 - Page Not Found | Hasura" />
      <div className="flex w-7/12 justify-between">
        <div className="px-5 md:p-0">
          <h1 className="font-bold text-6xl">404</h1>
          <br />
          This page does not exist. Head back{' '}
          <Link to="/" onClick={resetCallback}>
            Home
          </Link>
          .
        </div>
        <div className="w-1/3">
          <img
            src={errorImage}
            title="We think you are lost!"
            alt="Not found"
          />
        </div>
      </div>
    </div>
  );
};

export default connect()(PageNotFound);
