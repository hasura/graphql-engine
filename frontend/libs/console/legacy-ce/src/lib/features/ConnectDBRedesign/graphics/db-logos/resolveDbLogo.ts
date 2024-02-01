import defaultDbLogo from './default.svg';
import { dbLogos } from './dbLogos';

export const resolveDbLogo = (dbKind: string) => {
  const kind = dbKind.toLowerCase();
  if (dbLogos[kind]) {
    return dbLogos[kind];
  } else {
    const fuzzyFind = Object.keys(dbLogos).find(
      key => key.includes(kind) || kind.includes(key)
    );

    if (fuzzyFind) {
      return dbLogos[fuzzyFind];
    }
    return defaultDbLogo;
  }
};
