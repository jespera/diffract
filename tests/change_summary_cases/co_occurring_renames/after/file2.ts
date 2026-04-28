export function loadTwo(uid: string) {
  return getUser(uid);
}

export function clearTwo() {
  return invalidate(1);
}
