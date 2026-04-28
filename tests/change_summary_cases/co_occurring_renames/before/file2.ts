export function loadTwo(uid: string) {
  return fetchUser(uid);
}

export function clearTwo() {
  return resetCache(1);
}
