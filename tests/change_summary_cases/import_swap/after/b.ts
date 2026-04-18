import { logger } from "./new-logger";

export function farewell(name: string) {
  logger.info("bye " + name);
}
