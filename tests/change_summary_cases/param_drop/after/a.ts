function handleRequest(req: Request) {
  const result = process(req, defaultOptions);
  return result;
}
