import { legacyHandler } from "./legacy";

export function handleC(items: string[]) {
  return items.map((item) => legacyHandler(item));
}
