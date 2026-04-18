import { Cache } from "./cache";

const tokenCache = new Cache<string, Token>();

export function getToken(userId: string): Token | undefined {
  return tokenCache.get(userId);
}

export function storeToken(userId: string, token: Token): void {
  tokenCache.set(userId, token);
}

interface Token {
  value: string;
  expiresAt: number;
}
