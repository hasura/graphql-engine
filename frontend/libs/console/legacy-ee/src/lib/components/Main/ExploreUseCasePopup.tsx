import React from 'react';
import {
  Analytics,
  Button,
  getLSItem,
  LS_KEYS,
  setLSItem,
} from '@hasura/console-legacy-ce';
import { MdClose } from 'react-icons/md';

const ExploreUseCasePopup = () => {
  const showUseCaseOverviewPopup = getLSItem(LS_KEYS.showUseCaseOverviewPopup);

  const [showUseCasePopup, setShowUseCasePopup] =
    React.useState<boolean>(false);

  React.useEffect(() => {
    setShowUseCasePopup(showUseCaseOverviewPopup === 'true');
  }, [showUseCaseOverviewPopup]);

  const useCaseOverviewDocsUrl =
    'https://hasura.io/docs/latest/resources/use-case/overview/';

  return (
    showUseCasePopup && (
      <div className="z-[10] fixed w-96 bottom-14 right-12 border border-slate-300 abc">
        <div className="p-sm flex space-x-1.5 bg-emerald-50 justify-between">
          Want to learn more? Find out how you can leverage Hasura! 🚀
          <Analytics name="use-case-popup-dismiss">
            <MdClose
              className="mt-1 cursor-pointer"
              onClick={() => {
                setShowUseCasePopup(false);
                setLSItem(LS_KEYS.showUseCaseOverviewPopup, 'false');
              }}
            />
          </Analytics>
        </div>
        <div className="p-sm bg-slate-50 border-t border-slate-300">
          <Analytics name="use-case-popup-explore" passHtmlAttributesToChildren>
            <Button
              mode="default"
              className="w-full"
              onClick={() => {
                setShowUseCasePopup(false);
                setLSItem(LS_KEYS.showUseCaseOverviewPopup, 'false');
                window.open(
                  useCaseOverviewDocsUrl,
                  '_blank',
                  'noreferrer,noopener'
                );
              }}
            >
              Explore
            </Button>
          </Analytics>
        </div>
      </div>
    )
  );
};

export default ExploreUseCasePopup;
