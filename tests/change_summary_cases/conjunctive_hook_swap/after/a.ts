import { useUser } from "app/UserContext";

export function useProfile() {
  const { user: profile } = useUser();
  return profile;
}
