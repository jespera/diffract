import React, {
  ReactNode,
  useCallback,
  useEffect,
  useMemo,
  useState,
} from "react";

export const WidgetView = ({ children }: { children: ReactNode }) => {
  const [open, setOpen] = useState(false);
  useEffect(() => {
    setOpen(true);
  }, []);
  const cls = useMemo(() => (open ? "open" : "closed"), [open]);
  return <section className={cls}>{children}</section>;
};
