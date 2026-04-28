export function loadA(key: string) {
  return cache.read(key);
}

export function saveA(key: string, value: string) {
  cache.write(key, value);
}
