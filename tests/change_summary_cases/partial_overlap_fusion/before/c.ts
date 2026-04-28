export function loadC(key: string) {
  return cache.read(key);
}

export function saveC(key: string, value: string) {
  cache.write(key, value);
}
