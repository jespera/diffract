export function loadOne(id: string) {
  return getUser(id);
}

export function clearOne() {
  return invalidate(0);
}
