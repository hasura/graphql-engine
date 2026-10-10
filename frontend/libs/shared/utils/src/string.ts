export const matchAll = (re: RegExp, str: string): string[] => {
  let match: RegExpExecArray | null;
  const matches: string[] = [];

  while ((match = re.exec(str)) !== null) {
    // ref : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec
    matches.push(match[0]);
  }

  return matches;
};

export const replaceAll = (
  input: string,
  match: string | RegExp,
  replace: string,
) => {
  if (typeof input.replaceAll === 'function') {
    return input.replaceAll(match, replace);
  }

  return input.replace(
    typeof match === 'string' ? new RegExp(match, 'g') : match,
    replace,
  );
};

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

// from https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest#converting_a_digest_to_a_hex_string
export const hashString = async (
  str: string,
  algorithm?: AlgorithmIdentifier,
) => {
  const msgUint8 = new TextEncoder().encode(str); // encode as (utf-8) Uint8Array
  const hashBuffer = await crypto.subtle.digest(
    algorithm || 'SHA-256',
    msgUint8,
  ); // hash the message
  const hashArray = Array.from(new Uint8Array(hashBuffer)); // convert buffer to byte array
  const hashHex = hashArray
    .map((b) => b.toString(16).padStart(2, '0'))
    .join(''); // convert bytes to hex string
  return hashHex;
};

// return number with commas for readability
export const getReadableNumber = (number: number): string => {
  return number.toLocaleString();
};

export const capitalizeFirstLetter = (val: string) =>
  `${val[0].toUpperCase()}${val.slice(1)}`;

export const getIngForm = (value: string) => {
  return (
    (value[value.length - 1] === 'e'
      ? value.slice(0, value.length - 1)
      : value) + 'ing'
  );
};

export const getEdForm = (value: string) => {
  return (
    (value[value.length - 1] === 'e'
      ? value.slice(0, value.length - 1)
      : value) + 'ed'
  );
};
