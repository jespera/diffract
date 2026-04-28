export function loadC(key: string) {
  return cache.get(key);
}

export function saveC(key: string, value: string) {
  cache.set(key, value);
}
