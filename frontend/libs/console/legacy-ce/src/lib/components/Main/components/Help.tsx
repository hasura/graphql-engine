import clsx from 'clsx';
import React from 'react';
import { FaQuestionCircle } from 'react-icons/fa';
import { Link } from 'react-router';
import {
  activeLinkStyle,
  itemContainerStyle,
  linkStyle,
} from '../HeaderNavItem';

export const Help = ({ isSelected }: { isSelected: boolean }) => {
  return (
    <div className={itemContainerStyle}>
      <Link
        id="help"
        className={clsx(linkStyle, isSelected && activeLinkStyle)}
        to="/support/forums/"
      >
        <span className="text-sm">
          <FaQuestionCircle />
        </span>
        <span className="uppercase">HELP</span>
      </Link>
    </div>
  );
};
