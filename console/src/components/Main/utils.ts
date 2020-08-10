const proClickState = 'console:pro';
const defaultProClickState = {
  isProClicked: false,
};

const setProClickState = (proStateData: { isProClicked: boolean }) => {
  window.localStorage.setItem(proClickState, JSON.stringify(proStateData));
};

const getProClickState = () => {
  try {
    const p = window.localStorage.getItem(proClickState);

    if (p) {
      return JSON.parse(p);
    }

    window.localStorage.setItem(
      proClickState,
      JSON.stringify(defaultProClickState)
    );

    return defaultProClickState;
  } catch (e) {
    console.error(e);
    return defaultProClickState;
  }
};

const getReadAllNotificationsState = () => {
  return {
    read: 'all',
    date: new Date().toISOString(),
    showBadge: false,
  };
};

export { getProClickState, setProClickState, getReadAllNotificationsState };
