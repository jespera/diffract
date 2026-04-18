import { Request, Response } from "express";
import { logger } from "./logger";

export function getUser(req: Request, res: Response) {
  const id = req.params.id;
  const user = store.get(id);
  logger.info("fetched user", user);
  return res.json(user);
}

export function getUsers(req: Request, res: Response) {
  const ids = req.query.ids as string[];
  const users = ids.map((id) => store.get(id));
  logger.info("fetched users", users.length);
  return res.json(users);
}
