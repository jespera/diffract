import React, { useCallback, useMemo } from "react";

export const UserList = ({ users }: { users: string[] }) => {
  const sorted = useMemo(() => [...users].sort(), [users]);
  return (
    <ul>
      {sorted.map((u) => (
        <li key={u}>{u}</li>
      ))}
    </ul>
  );
};
