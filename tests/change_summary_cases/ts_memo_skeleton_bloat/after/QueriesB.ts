import { useMemo } from "react";

declare const records: { name: string; active: boolean }[];

export const useActiveNames = () => {
  const names = useMemo(() => {
    const active = records.filter((r) => r.active);
    const sorted = active.map((r) => r.name);
    sorted.sort();
    return sorted;
  }, [records]);
  return names;
};
