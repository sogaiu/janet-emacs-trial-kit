(declare-project
  :name "janet-emacs-trial-kit"
  :description "A Way to Try Emacs with Janet Support"
  :url "https://github.com/sogaiu/janet-emacs-trial-kit"
  :repo "git+https://github.com/sogaiu/janet-emacs-trial-kit")

(def tsm-ref
  "1b0a57c41c2760eaaa811ac415ded71ae06d2c1c")

(def proj-dir (os/cwd))

(put (dyn :rules) "test" nil)
(task "test" []
  (print "Sorry, no tests here, try `jpm tasks`."))

(defn ensure-bin
  [name]
  (try
    (zero? (let [f (file/temp)
                 r (os/execute ["which" name]
                               :px
                               {:out f})]
             (file/close f)
             r))
    ([e]
      (eprintf "%s does not appear to be on PATH" name)
      false)))

(defn get-version
  [cmd ver-peg]
  (try
    (let [f (file/temp)
          r (os/execute cmd
                        :px
                        {:out f})]
      (when (not (zero? r))
        (file/close f)
        (errorf "Non-zero exit code: %s" r))
      #
      (file/seek f :set 0)
      (def version-line (file/read f :line))
      (file/close f)
      (def version
        (-?>> (peg/match ver-peg version-line)
              first
              string/trimr))
      #
      version)
    ([e]
      (eprintf "Failed to determine version via %s" cmd)
      false)))

(defn get-major-version
  [ver-str]
  (-?>> (peg/match ~(sequence (capture :d+)) ver-str)
        first
        scan-number))

(task "setup" []
  # verify emacs exists and its version is sufficient
  (when (not (ensure-bin "emacs"))
    (eprintf "Failed to find emacs on PATH\n")
    (os/exit 1))
  (def version
    (get-version ["emacs" "--version"]
                 ~(sequence "GNU Emacs " (capture (to -1)))))
  (when (not version)
    (eprintf "Failed to determine emacs version\n")
    (os/exit 1))
  (def major-version
    (get-major-version version))
  (when (not (>= major-version 29))
    (eprintf "Emacs version must be 29 or above, found: %s\n" version)
    (os/exit 1))
  # ensure appropriate c compiler available
  (when (not (ensure-bin "cc"))
    (eprintf "Failed to find C compiler (cc) on PATH\n")
    (os/exit 1))
  # ensure tree-sitter grammar for janet is available
  (when (not (os/stat "tree-sitter-module/dist"))
    (print "Attempting to arrange for a janet tree-sitter grammar...")
    (def dir (os/cwd))
    (os/execute ["git"
                 "clone" "https://github.com/casouri/tree-sitter-module"]
                :p)
    (os/cd "tree-sitter-module")
    (os/execute ["git"
                 "checkout" tsm-ref]
                :p)
    (os/execute ["bash"
                 "build.sh" "janet-simple"]
                :p)
    (os/cd dir)))

(task "emacs" ["setup"]
  (os/execute ["emacs" "--init-directory=."]
              :p))

