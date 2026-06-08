import { useAppSelector } from "app/hooks";

export function useViewer() {
  const viewer = useAppSelector((state) => state.users.user);
  return viewer;
}
