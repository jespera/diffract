function B({icon, busy, run}: Props) {
  return (
    <Button
      icon={icon}
      priority="danger"
      disabled={busy}
      onClick={run}
    />
  );
}
