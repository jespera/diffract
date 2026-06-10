const x = useAppMemo(useFoo(), useCallback((m) => m.id, [dep]));
