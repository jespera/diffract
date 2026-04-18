function handleRequest(req: Request) {
  const result = process(req, defaultOptions, "verbose");
  return result;
}
