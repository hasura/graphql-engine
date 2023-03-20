export function jsonToString(value: any) {
  let inputValue = value;
  // If value is valid JSON, stringify it
  // Else, just use the value as is
  try {
    if (typeof value === 'string') {
      JSON.parse(value);
      inputValue = JSON.stringify(value);
    } else {
      inputValue = JSON.stringify(value);
    }
  } catch (error) {
    // Value is a string representing invalid json
    inputValue = value;
  }
  return inputValue;
}

export function stringToJson(value: string) {
  try {
    return JSON.parse(value);
  } catch (error) {
    // Value is invalid JSON, still we pass it as is so users can keep editing
    return value;
  }
}
