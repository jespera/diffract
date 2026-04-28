export function loadB(key: string) {
  return cache.read(key);
}

export function saveB(key: string, value: string) {
  cache.write(key, value);
}
