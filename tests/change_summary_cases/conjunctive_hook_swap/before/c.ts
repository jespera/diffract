import { useAppSelector } from "app/hooks";

export function useCurrent() {
  const current = useAppSelector((sel) => sel.users.user);
  return current;
}
