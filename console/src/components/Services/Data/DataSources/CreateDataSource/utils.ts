export const generateRandomString = (stringLength = 16) => {
  const allChars =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let str = '';

  for (let i = 0; i < stringLength; i++) {
    const randomNum = Math.floor(Math.random() * allChars.length);
    str += allChars.charAt(randomNum);
  }
  return str;
};
