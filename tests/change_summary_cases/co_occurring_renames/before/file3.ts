export function loadThree(key: string) {
  return fetchUser(key);
}

export function clearThree() {
  return resetCache(2);
}
