import { useUser } from "app/UserContext";

export function useCurrent() {
  const { user: current } = useUser();
  return current;
}
