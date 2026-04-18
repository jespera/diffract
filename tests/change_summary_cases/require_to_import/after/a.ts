import { logger } from "./logger";

export function greet(name: string) {
  logger.info("hello " + name);
}
