import React, { useEffect, useRef } from 'react';
import { Link } from 'react-router';

import styles from './Main.scss';
import arrowForwardBlock from './images/arrow_forward-block.svg';
import information from './images/information.svg';
import close from './images/x-circle.svg';
import ToolTip from '../Common/Tooltip/Tooltip';
import Toggle from '../Common/Toggle/Toggle';
import { closeIntercom, startIntercom } from './utils';
import { useToggle } from '../../hooks/useToggle';
import { useOnClickOutside } from '../../hooks/useOnClickOutside';

export const HelpPopup: React.FC = () => {
  const [isPopupOpen, togglePopupOpen] = useToggle(false);
  const [isChatOpen, toggleChatOpen] = useToggle(false);

  const popupRef = useRef<HTMLDivElement>(null);
  useOnClickOutside(popupRef, togglePopupOpen);

  useEffect(() => {
    if (isChatOpen) startIntercom();
    else closeIntercom();
  }, [isChatOpen]);

  return (
    <>
      <span onClick={togglePopupOpen}>HELP</span>
      {isPopupOpen && (
        <div className={styles.helpPopUpWrapper} ref={popupRef}>
          <img className={styles.helpPopClose} src={close} alt="Close" />
          <ul>
            <li>
              Enable Live Chat
              <ToolTip
                id="intercom-information"
                placement="bottom"
                message="By enabling live chat, we will inable Intercom for you on the console. No other data will be shared with this service. You can always enable & disable this feature as required."
              >
                <img src={information} alt="Information" />
              </ToolTip>
              <div
                className={`${styles.liveChatModeToggle} ${styles.cursorPointer}`}
              >
                <Toggle
                  checked={isChatOpen}
                  className={`${styles.display_flex}`}
                  onChange={toggleChatOpen}
                  icons={false}
                />
              </div>
            </li>
            <li>
              <Link to="/support/forum/">
                Support Forums <img src={arrowForwardBlock} alt="Arrow" />
              </Link>
            </li>
          </ul>
        </div>
      )}
    </>
  );
};
