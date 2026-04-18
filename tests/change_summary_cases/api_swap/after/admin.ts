import { Request, Response } from "express";
import { requireAdmin } from "./auth";

export function getAdminReport(req: Request, res: Response) {
  requireAdmin(req);
  const id = req.params.id;
  const report = store.get(id);
  return res.json(report);
}
