const logger = require("./logger");

export function farewell(name: string) {
  logger.info("bye " + name);
}
