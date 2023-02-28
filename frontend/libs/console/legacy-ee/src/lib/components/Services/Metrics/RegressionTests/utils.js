import React from 'react';
import moment from 'moment-timezone';

import { stripUnderScore, capitalize } from '../Operations/utils';
import {
  SUCCESS_ENUM,
  FAILED_ENUM,
  IN_PROCESS_ENUM,
  QUEUED_ENUM,
  CANCELED_ENUM,
} from './constants';
import Globals from '../../../../Globals';
import LoadingSpinner from '../Common/LoadingSpinner';
import success from '../images/success.svg';
import failure from '../images/failure.svg';
import queue from '../images/queue.svg';
import cancel from '../images/cancel-metrics.svg';

export const getSchemeLuxUrl = () => {
  const { hasuraOAuthUrl } = Globals;
  if (!hasuraOAuthUrl) {
    return {
      scheme: 'http',
      url: 'lux-dev.hasura.me',
    };
  }
  const replacedUrl = hasuraOAuthUrl.replace('oauth', '').split('://.');
  return {
    scheme: replacedUrl[0] || 'http',
    url: replacedUrl[1] || 'lux-dev.hasura.me',
  };
};

const MAX_WIDTH = 600;
const HEADER_PADDING = 62;
const CONTENT_PADDING = 36;
const HEADER_FONT = 'bold 16px Gudea';
const CONTENT_FONT = '14px Gudea';

export const PAGE_LIMIT = 10;

export const transformMessage = row => {
  if (row.status === SUCCESS_ENUM) {
    return 'Passed';
  } else if (row.status === FAILED_ENUM) {
    if (
      typeof row.message === 'object' &&
      'errors' in row.message &&
      row.message.errors.length > 0
    ) {
      return row.message.errors[0].message;
    }
  }
  return JSON.stringify(row.message) || 'N/A';
};

/**
 * @param {string} value
 */
export const getTitle = value => {
  return capitalize(stripUnderScore(value));
};

/**
 *
 * @param {string} text
 * @param {string} font
 */
const getTextWidth = (text, font) => {
  const canvas =
    getTextWidth.canvas ||
    (getTextWidth.canvas = document.createElement('canvas'));

  const context = canvas.getContext('2d');
  context.font = font;

  const metrics = context.measureText(text);
  return metrics.width;
};

/**
 *
 * @param {string} header
 * @param {{[key in header]: string | {}}[]} contentRows
 */
export const getColWidth = (header, contentRows = []) => {
  let maxContentWidth = 0;
  contentRows.forEach(row => {
    if (row && row[header]) {
      const content = row[header];
      let contentString;
      if (header === 'message') {
        contentString = transformMessage(row);
      } else if (typeof content === 'object') {
        contentString = JSON.stringify(content, null, 4);
      } else if (header === 'last_seen') {
        contentString = moment(content).fromNow();
      } else {
        contentString = content.toString();
      }

      const currLength = getTextWidth(contentString, CONTENT_FONT);
      if (currLength > maxContentWidth) {
        maxContentWidth = currLength;
      }
    }
  });

  const maxContentCellWidth = maxContentWidth + CONTENT_PADDING + 12;
  const headerCellWidth =
    getTextWidth(getTitle(header), HEADER_FONT) + HEADER_PADDING;

  return Math.min(MAX_WIDTH, Math.max(maxContentCellWidth, headerCellWidth));
};

export const DataNotAvaliable = ({ children }) => (
  <div>
    There are no operations in this list.
    {children}
  </div>
);

export const getTestRunStatusText = runData => {
  if (runData.status === IN_PROCESS_ENUM) {
    return 'Tests are queued';
  }
  const timezone = Intl.DateTimeFormat().resolvedOptions().timeZone;
  const datetime = moment(runData.finished_at)
    .tz(timezone)
    .format('Do MMMM YYYY h:mma z');
  switch (runData.status) {
    case FAILED_ENUM:
      const errorsCount = runData.test_run_details.filter(
        d => d.status === FAILED_ENUM
      ).length;
      return `Ran with ${errorsCount} ${
        errorsCount === 1 ? 'error' : 'errors'
      } on ${datetime}`;
    case SUCCESS_ENUM:
      return `Ran successfully on ${datetime}`;

    default:
      return '';
  }
};

export const StatusIcon = ({ status }) => {
  switch (status) {
    case QUEUED_ENUM:
      return <img src={queue} alt="Queued" style={{ opacity: '0.6' }} />;
    case IN_PROCESS_ENUM:
      return <LoadingSpinner />;
    case SUCCESS_ENUM:
      return <img src={success} alt="Success" />;
    case FAILED_ENUM:
      return <img src={failure} alt="Failure" />;
    case CANCELED_ENUM:
      return <img src={cancel} alt="Canceled" style={{ opacity: '0.6' }} />;
    default:
      return null;
  }
};
