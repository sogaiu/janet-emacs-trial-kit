# this file is used by multiple janet-*-trial-kits

(defn ensure-bin
  [name]
  (try
    (with [f (file/temp)]
      (zero? (os/execute ["which" name] :px {:out f})))
    ([e]
      (eprintf "%s does not appear to be on PATH" name)
      false)))

(defn ensure-win-bin
  [name]
  (try
    (with [f (file/temp)]
      (zero? (os/execute ["where" name] :px {:out f})))
    ([e]
      (eprintf "%s does not appear to be on PATH" name)
      false)))

(defn get-version
  [cmd ver-peg]
  (try
    (with [f (file/temp)]
      (def r (os/execute cmd :px {:out f}))
      (when (not (zero? r))
        (errorf "Non-zero exit code: %s" r))
      #
      (file/seek f :set 0)
      (def version-line (file/read f :line))
      (def version
        (-?>> (peg/match ver-peg version-line)
              first
              string/trimr))
      #
      version)
    ([e]
      (eprintf "Failed to determine version via %s" cmd)
      false)))

(defn have-cc?
  []
  (if (= :windows (os/which))
    (when (and (not (ensure-win-bin "gcc.exe"))
               (not (ensure-win-bin "clang.exe")))
      (eprintf "Failed to find gcc.exe or clang.exe on PATH\n")
      (break false))
    (when (not (ensure-bin "cc"))
      (eprintf "Failed to find C compiler (cc) on PATH\n")
      (break false)))
  #
  true)
