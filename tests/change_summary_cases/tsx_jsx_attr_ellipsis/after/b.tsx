function B({icon, busy, run}: Props) {
  return (
    <Button
      icon={icon}
      variant="danger"
      disabled={busy}
      onClick={run}
    />
  );
}
