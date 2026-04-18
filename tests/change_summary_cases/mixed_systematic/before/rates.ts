import { Cache } from "./cache";

const rateCache = new Cache<string, number>();

export function getRate(currency: string): number | undefined {
  return rateCache.read(currency);
}

export function setRate(currency: string, rate: number): void {
  if (rate < 0) {
    throw new Error("rate must be non-negative");
  }
  rateCache.write(currency, rate);
}
