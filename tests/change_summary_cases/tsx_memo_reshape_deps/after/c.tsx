const z = useAppMemo(useBaz(), useCallback((m) => m.val, [d, e, f]));
