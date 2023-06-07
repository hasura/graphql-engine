import { ReactNode, createContext, useCallback, useContext } from 'react';

export type Feature = 'exists';

const forbiddenFeaturesContext = createContext<{
  forbidden: Feature[];
  hasFeature: (feature: Feature) => boolean;
}>({
  forbidden: [],
  hasFeature: () => {
    return true;
  },
});

export function useForbiddenFeatures() {
  return useContext(forbiddenFeaturesContext);
}

export function ForbiddenFeaturesProvider({
  forbidden = [],
  children,
}: {
  forbidden: Feature[] | undefined;
  children: ReactNode;
}) {
  const hasFeature = useCallback(
    function hasFeature(feature: Feature) {
      return !forbidden.includes(feature);
    },
    [forbidden]
  );
  return (
    <forbiddenFeaturesContext.Provider
      value={{ forbidden: forbidden, hasFeature }}
    >
      {children}
    </forbiddenFeaturesContext.Provider>
  );
}
