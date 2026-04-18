import { Cache } from "./cache";

const tokenCache = new Cache<string, Token>();

export function getToken(userId: string): Token | undefined {
  return tokenCache.read(userId);
}

export function storeToken(userId: string, token: Token): void {
  tokenCache.write(userId, token);
}

interface Token {
  value: string;
  expiresAt: number;
}
