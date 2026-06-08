import { useAppSelector } from "app/hooks";

export function useProfile() {
  const profile = useAppSelector((s) => s.users.user);
  return profile;
}
