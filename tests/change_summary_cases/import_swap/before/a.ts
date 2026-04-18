import { logger } from "./old-logger";

export function greet(name: string) {
  logger.info("hello " + name);
}
