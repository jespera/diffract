import { logger } from "./old-logger";

export function farewell(name: string) {
  logger.info("bye " + name);
}
