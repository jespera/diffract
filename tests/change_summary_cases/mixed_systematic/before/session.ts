import { Cache } from "./cache";

const cache = new Cache<string, Session>();

export function load(id: string): Session | undefined {
  return cache.read(id);
}

export function save(id: string, session: Session): void {
  cache.write(id, session);
}

interface Session {
  userId: string;
  expiresAt: number;
}
