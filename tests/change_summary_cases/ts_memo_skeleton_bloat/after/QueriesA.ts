import { useMemo } from "react";

declare const identities: [string, string[]][];

export const useDepoBanks = () => {
  const depoBanks = useMemo(() => {
    const listed = identities.map(([id]) => id);
    return listed.filter((x) => x.length > 0);
  }, [identities]);
  return depoBanks;
};
