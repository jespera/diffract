import React, { useEffect, useMemo, useState } from "react";

export const UserPanel = ({ id }: { id: string }) => {
  const [name, setName] = useState("");
  useEffect(() => {
    setName(id.toUpperCase());
  }, [id]);
  const label = useMemo(() => `user: ${name}`, [name]);
  return <div>{label}</div>;
};
