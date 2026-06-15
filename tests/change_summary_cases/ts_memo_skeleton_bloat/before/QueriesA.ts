import { useMemo } from "react";

declare const identities: [string, string[]][];

export const useDepoBanks = () => {
  const depoBanks = useMemo(() => identities.map(([id]) => id), [identities]);
  return depoBanks;
};
