import { Request, Response } from "express";
import { logger } from "./logger";
import { validate } from "./validate";

export function getOrder(req: Request, res: Response) {
  const id = req.params.id;
  if (!validate(id)) {
    return res.status(400).send("invalid id");
  }
  const order = store.get(id);
  return res.json(order);
}

export function cancelOrder(req: Request, res: Response) {
  const id = req.params.id;
  const order = store.get(id);
  order.status = "cancelled";
  return res.json(order);
}
