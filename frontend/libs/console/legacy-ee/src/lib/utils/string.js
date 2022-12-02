export const byteCount = s => {
  return encodeURI(s).split(/%..|./).length - 1;
};
