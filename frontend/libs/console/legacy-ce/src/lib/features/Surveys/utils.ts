interface ArrayItem {
  position: number;
}

/**
 * This function returns -1, 0, or 1 randomly
 */
function getRandomInt() {
  return Math.floor(Math.random() * 3) - 1;
}

/**
 * This function orders the array items in the following order.
 * If the `position` key is different for two items `a` and `b`, we sort them in ascending order.
 * If the `position` key is same for two items `a` and `b`, we shuffle them in random order.
 */
export function orderArrayItems<T extends ArrayItem>(unroderedArray: T[]) {
  const orderedArray = unroderedArray.sort((a, b) => {
    if (a.position === b.position) {
      return getRandomInt();
    } else return a.position - b.position;
  });

  return orderedArray;
}
