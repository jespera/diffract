import { logger } from "./new-logger";

export function greet(name: string) {
  logger.info("hello " + name);
}
