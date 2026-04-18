import { legacyHandler } from "./legacy";

export function handleB(input: Buffer) {
  const parsed = legacyHandler(input);
  return parsed;
}
