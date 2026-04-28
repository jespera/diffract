export function loadThree(key: string) {
  return getUser(key);
}

export function clearThree() {
  return invalidate(2);
}
