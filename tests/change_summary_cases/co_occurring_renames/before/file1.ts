export function loadOne(id: string) {
  return fetchUser(id);
}

export function clearOne() {
  return resetCache(0);
}
