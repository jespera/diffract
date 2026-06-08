import { useUser } from "app/UserContext";

export function useViewer() {
  const { user: viewer } = useUser();
  return viewer;
}
