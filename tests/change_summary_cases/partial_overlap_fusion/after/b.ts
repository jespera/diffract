export function loadB(key: string) {
  return cache.get(key);
}

export function saveB(key: string, value: string) {
  cache.set(key, value);
}
