import { legacyHandler } from "./legacy";

export function handleA(req: Request) {
  const result = legacyHandler(req);
  return result;
}
