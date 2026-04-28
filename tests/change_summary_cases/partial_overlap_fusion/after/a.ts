export function loadA(key: string) {
  return cache.get(key);
}

export function saveA(key: string, value: string) {
  cache.set(key, value);
}
