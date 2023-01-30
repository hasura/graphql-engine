// a slight artificial delay can improve UX.
// use Promise.all so we don't force the user to wait longer than necessary
async function delayAsyncAction<T>(asyncAction: Promise<T>, delay = 300) {
  const [res] = await Promise.all([
    asyncAction,
    new Promise(resolve => setTimeout(resolve, delay)),
  ]);

  return res;
}

export default delayAsyncAction;
