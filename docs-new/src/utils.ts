export const setVar = (key, value) => document.documentElement.style.setProperty(key, value);

export const getVar = (key) => getComputedStyle(document.documentElement).getPropertyValue(key);