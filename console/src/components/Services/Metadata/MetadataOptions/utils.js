// return time in format YYYY_MM_DD_hh_mm_ss_s
export const getCurrTimeForFileName = () => {
  const currTime = new Date();

  const year = currTime
    .getFullYear()
    .toString()
    .padStart(4, '0');

  const month = (currTime.getMonth() + 1).toString().padStart(2, '0');

  const date = currTime
    .getDate()
    .toString()
    .padStart(2, '0');

  const hours = currTime
    .getHours()
    .toString()
    .padStart(2, '0');

  const minutes = currTime
    .getMinutes()
    .toString()
    .padStart(2, '0');

  const seconds = currTime
    .getSeconds()
    .toString()
    .padStart(2, '0');

  const milliSeconds = currTime
    .getMilliseconds()
    .toString()
    .padStart(3, '0');

  return [year, month, date, hours, minutes, seconds, milliSeconds].join('_');
};
