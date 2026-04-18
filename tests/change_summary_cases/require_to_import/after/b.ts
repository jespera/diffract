import { logger } from "./logger";

export function farewell(name: string) {
  logger.info("bye " + name);
}
