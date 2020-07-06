import { setLSItem, getLSItem } from '../../utils/localstorage';

const proClickState = 'console:pro';
const defaultProClickState = {
  isProClicked: false,
};

const setProClickState = (proStateData: { isProClicked: boolean }) => {
  setLSItem(proClickState, JSON.stringify(proStateData));
};

const getProClickState = () => {
  try {
    const proState = getLSItem(proClickState);

    if (proState) {
      return JSON.parse(proState);
    }

    setLSItem(proClickState, JSON.stringify(defaultProClickState));

    return defaultProClickState;
  } catch (e) {
    console.error(e);
    return defaultProClickState;
  }
};

export { getProClickState, setProClickState };
