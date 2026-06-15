import { useMemo } from "react";

declare const records: { name: string; active: boolean }[];

export const useActiveNames = () => {
  const names = useMemo(() => records.map((r) => r.name), [records]);
  return names;
};
