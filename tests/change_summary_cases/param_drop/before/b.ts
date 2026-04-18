export function run(input: Buffer) {
  const parsed = process(input, strictOptions, "quiet");
  return parsed;
}
