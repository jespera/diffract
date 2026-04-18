const logger = require("./logger");

export function greet(name: string) {
  logger.info("hello " + name);
}
