import PropTypes from 'prop-types';
import React from 'react';
// import { vSetDefaults, vMakeRequest, vExpandHeading } from './ViewActions'; // eslint-disable-line no-unused-vars
// import { setTrigger } from '../EventActions';
import TableHeader from './TableHeader';

import ViewRows from './ViewRows';

// import ViewRows from './ViewRows';
// import { NotFoundError } from '../../../Error/PageNotFound';

/*
const genHeadings = headings => {
  if (headings.length === 0) {
    return [];
  }

  const heading = headings[0];
  if (typeof heading === 'string') {
    return [heading, ...genHeadings(headings.slice(1))];
  }
  if (typeof heading === 'object') {
    if (!heading._expanded) {
      const headingName =
        heading.type === 'obj_rel' ? heading.lcol : heading.relname;
      return [
        { name: headingName, type: heading.type },
        ...genHeadings(headings.slice(1)),
      ];
    }
    if (heading.type === 'obj_rel') {
      const subheadings = genHeadings(heading.headings).map(h => {
        if (typeof h === 'string') {
          return heading.relname + '.' + h;
        }
        return heading.relname + '.' + h.name;
      });
      return [...subheadings, ...genHeadings(headings.slice(1))];
    }
  }

  throw 'Incomplete pattern match'; // eslint-disable-line no-throw-literal
};

const genRow = (row, headings) => {
  if (headings.length === 0) {
    return [];
  }

  const heading = headings[0];
  if (typeof heading === 'string') {
    return [row[heading], ...genRow(row, headings.slice(1))];
  }
  if (typeof heading === 'object') {
    if (!heading._expanded) {
      const rowVal = heading.type === 'obj_rel' ? row[heading.lcol] : '[...]';
      return [rowVal, ...genRow(row, headings.slice(1))];
    }
    if (heading.type === 'obj_rel') {
      const subrow = genRow(row[heading.relname], heading.headings);
      return [...subrow, ...genRow(row, headings.slice(1))];
    }
  }

  throw 'Incomplete pattern match'; // eslint-disable-line no-throw-literal
};
*/

const ViewScheduledTrigger = props => {
  const { dispatch } = props;
  // Choose the right nav bar header thing
  const header = (
    <TableHeader dispatch={dispatch} tabName="scheduledTriggers" />
  );

  return (
    <div>
      {header}
      <br />
      <ViewRows dispatch={dispatch} />
    </div>
  );
};

ViewScheduledTrigger.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state,
  };
};

const viewScheduledTriggerConnector = connect =>
  connect(mapStateToProps)(ViewScheduledTrigger);

export default viewScheduledTriggerConnector;
