/* eslint-disable jsx-a11y/anchor-is-valid */
import { CloseCircleFilled } from '@ant-design/icons';
import { useLocalStorage } from '../../hooks';
import ToolTip from '../Common/Tooltip/Tooltip';
import {
  sendTelemetryEvent,
  telemetryUserEventsTracker,
} from '../../telemetry';
import { InitializeTelemetry } from '../../features/Analytics';
import { FiExternalLink } from 'react-icons/fi';

export const DDNBanner = () => {
  const [isDismissed, setIsDimissed] = useLocalStorage(
    'console:dismiss-ddn-banner',
    false
  );

  if (isDismissed) return null;

  return (
    <>
      <InitializeTelemetry tracker={telemetryUserEventsTracker} skip={false} />
      <div
        style={{
          background: '#F0F4FF',
          border: '1px solid #C6D6FF',
          height: '2.5rem',
        }}
        className="flex content-center flex-wrap"
      >
        <div className="w-[90%] flex justify-center">
          <span role="img" aria-label="celebrate-emoji" className="mr-1">
            🎉
          </span>
          <span>
            Hasura DDN, our next-gen platform, is now live with new powerful
            features such as federation, lambda connectors, and faster CI/CD!
          </span>
          <span className="ml-xs">
            <button
              className="cursor-pointer underline flex items-center gap-1"
              style={{
                color: '#697187',
              }}
              onClick={() => {
                sendTelemetryEvent({
                  type: 'CLICK_EVENT',
                  data: {
                    id: 'learn-hasura-ddn',
                  },
                });
                window.open(
                  'https://hasura.io/why-upgrade-to-hasura-ddn?utm_source=hasura-ce&utm_medium=console&plcmt=banner-ticker'
                );
              }}
            >
              Learn more <FiExternalLink />
            </button>
          </span>
        </div>
        <div
          className="flex w-[10%] justify-end px-sm mt-[-5px]"
          style={{ alignItems: 'center' }}
        >
          <ToolTip message={"Don't show me this again"} placement="bottom">
            <CloseCircleFilled
              style={{ color: '#5d82e0a6' }}
              className="cursor-pointer hover:scale-110"
              onClick={() => {
                setIsDimissed(true);
              }}
            />
          </ToolTip>
        </div>
      </div>
    </>
  );
};
