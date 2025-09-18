{ pkgs, pkgsUnstable, nixpkgs, nixpkgsUnstable, doomemacs, nu-scripts, username, ... }@args:
let
  inherit (pkgs) lib stdenv;
  guiEnabled = args.guiEnabled or stdenv.hostPlatform.isDarwin;

  userName = "Dani Brown";
  userEmail = "d@dani.cool";
in
{
  imports = [
    ({ ... }@args: {
      config._module.args = {
        inherit guiEnabled;
      };
    })
    ./emacs.nix
    ./nushell-vterm
    (import ./channel-pins.nix { inherit nixpkgs nixpkgsUnstable; })
    (import ./registry-pins.nix { inherit nixpkgs nixpkgsUnstable; })
    ./linkapps.nix
  ];

  home.stateVersion = "23.11"; # XXX: remember, don't change!

  home.username = username;
  home.homeDirectory = if stdenv.hostPlatform.isDarwin
                       then "/Users/${username}"
                       else "/home/${username}";

  home.language.base = "en_GB.UTF-8";

  home.sessionPath = [
    "$HOME/.local/bin" # uv at least puts stuff in here
  ];

  home.packages = (with pkgs; [
    aws-vault
    fd
    git-absorb
    git-extras
    graphviz
    home-manager
    htop
    (iosevka.override {
      privateBuildPlan = builtins.readFile ./iosevka-private-build-plans.toml;
      set = "DWB";
    })
    (iosevka.override {
      privateBuildPlan = builtins.readFile ./iosevka-term-private-build-plans.toml;
      set = "DWBTerm";
    })
    jc # makes JSON out of standard commands, used in my nushell utils
    mosh
    nixd
    nixfmt-rfc-style
    # nodejs_22 # mainly for emacs copilot but generally useful too ig
    plantuml
    python3
    ripgrep
    rsync
    (ruby_3_4.withPackages (ps: with ps; [
      faraday
      nokogiri
      pry
    ]))
    wget
    zstd
  ]) ++ (with pkgsUnstable; [
    # ghostty ## fucks sake marked broken on darwin
    aider-chat
    delta # diff formatter
    delve
    go
    gopls
    (symlinkJoin {
      name = "gotools";
      paths = [ gotools ];
      # clashes with ruby bundler!
      postBuild = ''
        mv $out/bin/bundle $out/bin/gobundle
      '';
    })

  ]) ++ lib.optionals stdenv.hostPlatform.isDarwin (with pkgs; [
    reattach-to-user-namespace
  ]);

  fonts.fontconfig.enable = true;

  home.file.nushell-my-scripts = {
    source = ./conf/nushell;
    target = ".config/nushell/scripts/my";
  };

  home.file.nushell-contrib-custom-completions-scripts = {
    source = "${nu-scripts}/custom-completions";
    target = ".config/nushell/scripts/contrib/custom-completions";
  };

  home.file.nushell-contrib-nu-hooks-scripts = {
    source = "${nu-scripts}/nu-hooks";
    target = ".config/nushell/scripts/contrib/nu-hooks";
  };

  home.file.editrc = {
    target = ".editrc";
    text = ''
      bind -v
    '';
  };

  home.file.".config/ghostty/config" = {
    text = ''
      font-family = "Iosevka Term SS08"
      font-size = 12
      window-theme = light
      theme = light:zenwritten_light,dark:zenwritten_dark
      auto-update = off
    '';
  };

  home.file.haskeline = {
    target = ".haskeline";
    text = ''
      editMode: Vi
    '';
  };

  home.file.psqlrc = {
    target = ".psqlrc";
    text = ''
      \pset linestyle unicode
      \pset border 2
      \pset null ␀
      \set PROMPT1 '%[%033[33;1m%]%x%[%033[0m%]%[%033[1m%]%/%[%033[0m%]%R%# '
      \timing
      \x auto
    '';
  };

  home.file.".local/share/my-ollama-models" = {
    source = ./conf/ollama-models;
  };

  home.shellAliases = {
    g = "git";
    ll = "ls -l";
    la = "ls -la";
  };

  programs.carapace = {
    enable = true;
    package = pkgsUnstable.carapace;
  };

  programs.direnv = {
    enable = true;
    enableNushellIntegration = true;
    enableFishIntegration = false;
    nix-direnv.enable = true;
  };

  programs.git = {
    enable = true;
    ## XXX: not this because it uses openssh from nixpkgs, which
    ## doesn't include keychain integration.
    # package = pkgs.gitAndTools.gitFull;

    package = pkgsUnstable.git;

    inherit userName userEmail;

    ignores = [
      ".DS_Store"
      "*~"
      ".tmp/"
      ".dir-locals-2.el"
      ".direnv/"
    ];

    aliases = {
      a = "annex";
      aa = "add --all";
      ai = "add -i";
      ap = "add -p";
      ar = "add --update :/";
      ara = "add --all :/";
      as = "!\"f() { git ls-files -m | grep $1 | xargs git add; }; f\"";
      au = "add --update";
      br = "branch";
      branch-delete-merged = "!git branch --merged | grep -v '\\*' | xargs -n 1 git branch -d";
      brn = "rev-parse --abbrev-ref HEAD";
      changelog = "log --reverse '--format=* %s (%an)'";
      ci = "commit -v";
      cia = "commit -av";
      ciam = "commit -am";
      ciar = "!git ar && git ci";
      cim = "commit -m";
      cip = "commit -pv";
      co = "checkout";
      cob = "checkout -";
      com = "!git switch \"$(basename \"$(git rev-parse --abbrev-ref origin/HEAD)\")\"";
      commend = "commit --amend --no-edit";
      conflicts = "!git status --porcelain=2 | awk '$1 == \"u\" { print $11 }'";
      di = "diff";
      dic = "diff --cached";
      dim = "!set -e; upstream=$(git rev-parse --symbolic-full-name '@{upstream}'); git diff $(git merge-base --fork-point $upstream)..HEAD";
      fixauthor = "commit --amend --reset-author --no-edit";
      fixup = "!\"git commit --fixup $(git log --color=always --format='%C(auto)%h%d %s %C(black)%C(bold)%cr' | fzf --ansi --no-sort --reverse | awk '{print $1}')\"";
      graph = "log --graph --pretty=format':%C(yellow)%h%Cblue%d%Creset %s %C(white) %an, %ar%Creset'";
      grog = "log --graph --abbrev-commit --decorate --all --format=format:\\\"%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(dim white) - %an%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n %C(white)%s%C(reset)\\\"";
      ignore = "update-index --assume-unchanged";
      ignored = "!git ls-files -v | awk '/^h /{print $2}'";
      l = "log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      pb = "publish-branch";
      ph = "push";
      prr = "!git publish-branch --ignore-exists && hub browse -- \"compare/$(git rev-parse --abbrev-ref head)?expand=1\"";
      prune-squashed-branches = "!f() { set -e; git switch \"$1\"; git pull; git remote prune origin; git delete-squashed-branches --proceed; }; f \"$(basename \"$(git rev-parse --abbrev-ref origin/HEAD)\")\"";
      pu = "pull";
      pur = "pull --rebase";
      purph = "!git pull --rebase && git push";
      re = "rebase";
      root = "rev-parse --show-toplevel";
      sf = "status";
      sm = "submodule";
      squash = "rebase --autosquash";
      ss = "status -sb";
      st = "status -sb";
      sw = "switch";
      unignore = "update-index --no-assume-unchanged";
      unwip = "reset HEAD^";
      wip = "!git add --all :/ && git commit -n -m DANWIP";
    };

    difftastic.enable = true;
    lfs.enable = true;

    extraConfig = {
      core = {
        sparseCheckout = true;
        quotepath = false;
        pager = "less -XR";
        untrackedCache = true;
      };
      merge = {
        stat = true;
      };
      color.ui = "auto";
      diff = {
        mnemonicprefix = true;
        compactionHeuristic = true;
        colorMoved = "default";
      };
      push = {
        default = "current";
        autoSetupRemote = true;
      };
      status = {
        relativePaths = true;
        submoduleSummary = true;
      };
      credential = if stdenv.hostPlatform.isDarwin then {} else {
        helper = "osxkeychain";
      };
      branch.sort = "committerdate";
      github.user = "dwb";
      init.defaultBranch = "main";
      pull.ff = "only";
      rebase.autoSquash = true;
      rerere.enabled = true;

      # as in https://dandavison.github.io/delta
      # the diff formatter
      delta = {
        light = true;
        pager = "";
      };

      # this is too problematic
      # url."ssh://git@github.com/".insteadOf = "https://github.com/";
    };
  };

  programs.jujutsu = {
    enable = true;
    package = pkgsUnstable.jujutsu;
    settings = let
      privateCommits = "description(glob:'wip:*') | description(glob:'private:*')";
      delta = "${pkgsUnstable.delta}/bin/delta";
    in {
      user = {
        name = userName;
        email = userEmail;
      };
      diff = {
        tool = delta;
      };
      ui = {
        bookmark-list-sort-keys = ["committer-date"];
        default-command = ["log" "--reversed"];
        diff-editor = ":builtin";
        diff-formatter = ":git";
        pager = ":builtin";
        paginate = "auto";
      };
      git = {
        private-commits = privateCommits;
      };
      aliases = {
        di = ["diff"];
        # diff from trunk
        dt = ["diff" "-r" "trunk()..@"]
        # log to trunk
        lt = ["log" "--reversed" "-r" "parents(trunk()) | trunk()..@"];
        # (heads of) my branches (even ahead of bookmarks)
        mb = ["log" "--reversed" "-r" "visible_heads() & mine()::"];
        pre-commit = ["util" "exec" "--" "bash" "-c" ''
          set -euo pipefail

          EMPTY=$(jj log --no-graph -r @ -T 'empty')
          if [[ $EMPTY = "false" ]]; then echo "not on an empty revision"; exit 1; fi

          FROM=$(jj log --no-graph -r "fork_point(trunk() | @)" -T "commit_id")
          TO=$(jj log --no-graph -r "@" -T "commit_id")

          ROOT=$(jj root)
          if [[ -f $ROOT/.jj/repo ]]; then
            # we are in a workspace
            real_repo="$(< "$ROOT/.jj/repo")"
            export GIT_DIR="$(realpath "$real_repo/../../.git")"
            export GIT_WORK_TREE="$ROOT"
          fi

          pre-commit run --from="$FROM" --to="$TO" "$@"
        ''];
        # rebase on trunk
        retrunk = ["rebase" "-d" "trunk()"];
        # split before private (commits)
        sbp = ["split" "--insert-after" "heads((trunk()..@) ~ private_commits())"];
        tug = ["bookmark" "move" "--from" "closest_bookmark(@)" "--to" "closest_public_nonempty(@)"];
      };
      revset-aliases = {
        "closest_bookmark(to)" = "heads(::to & bookmarks())";
        "closest_nonempty(to)" = "heads(::to ~ empty())";
        "closest_public_nonempty(to)" = "closest_nonempty(to) ~ private_commits()";
        "immutable_heads()" = "builtin_immutable_heads() | tracked_remote_bookmarks() | (trunk().. & ~mine())";
        "private_commits()" = privateCommits;
      };
      "--scope" = [
        {
          "--when".commands = ["diff" "show"];
          ui.pager = delta;
        }
      ];
    };
  };

  programs.helix = {
    enable = true;
    package = pkgsUnstable.helix;
  };

  programs.nushell = {
    enable = true;
    package = pkgsUnstable.nushell;
    plugins = with pkgsUnstable.nushellPlugins; [
      (pkgsUnstable.callPackage (import ./pkgs/nushell-plugins-nupsql.nix) {})

      formats
      polars
      ## broken in unstable:
      # highlight
      # net
      # units
    ];
    configFile.text = lib.concatLines (
      # ["source ${nu-scripts}/themes/nu-themes/windows-highcontrast-light.nu"] ++
      (lib.pipe [
        ./conf/config.nu
        ./conf/local_config.nu
      ] [
        (lib.filter lib.pathExists)
        (map builtins.readFile)
      ]));
    envFile.text = lib.pipe [
      ./conf/env.nu
      ./conf/local_env.nu
    ] [
      (lib.filter lib.pathExists)
      (map builtins.readFile)
      lib.concatLines
    ];
    enableVtermIntegration = true;
  };

  programs.readline = {
    enable = true;
    variables = {
      editing-mode = "vi";
      keymap = "vi";
      enable-keypad = "on";
    };
  };

  programs.tmux = {
    enable = true;

    clock24 = true;
    escapeTime = 10; # ms, after which it's a person pressing Esc
    keyMode = "vi";
    mouse = true;
    newSession = true;
    shortcut = "a";

    extraConfig = ''
      # bind-key a send-key C-a
      # bind-key C-a last-pane
      bind-key A last-window
      bind-key C clear-history

      # setw -g remain-on-exit off
      # set-hook -g session-created 'set -g remain-on-exit off'

      set -g mouse on

      bind-key -T prefix s choose-tree -NsO time

      # act like vim
      setw -g mode-keys vi
      bind-key h select-pane -L
      bind-key j select-pane -D
      bind-key k select-pane -U
      bind-key l select-pane -R
      bind-key -r C-h select-window -t :-
      bind-key -r C-l select-window -t :+

      bind-key g copy-mode \; send-key g

      # Copy mode
      setw -g mode-keys vi
      unbind p
      bind-key p paste-buffer
      bind-key -T copy-mode-vi v send-keys -X begin-selection
      bind-key -T copy-mode-vi y send-keys -X copy-selection
      bind-key -T copy-mode-vi Escape send-keys -X cancel

      bind-key R respawn-pane
    '' + (lib.optionalString stdenv.hostPlatform.isDarwin
      (let
        rtun = "${pkgs.reattach-to-user-namespace}/bin/reattach-to-user-namespace";
      in ''
        bind-key -T copy-mode-vi y send-keys -X copy-pipe "${rtun} pbcopy"
        bind-key P run-shell "${rtun} pbpaste | tmux load-buffer - && tmux paste-buffer"
        bind-key y run "tmux save-buffer - | ${rtun} pbcopy"
      ''));
  };

  programs.vim = {
    enable = true;
    packageConfigurable = pkgs.vim-full.override {
      config = {
        vim = {
          gui = "none";
        };
      };
    };
    defaultEditor = !guiEnabled;
    extraConfig = builtins.readFile ./conf/vimrc.vim;
  };

  programs.zsh = {
    enable = true;
    initContent = lib.concatLines [
      (builtins.readFile ./conf/zshrc.zsh)
      (builtins.readFile ./conf/zshrc-local.zsh)
    ];
    envExtra = builtins.readFile ./conf/zshenv-local.zsh;
  };

  programs.fzf.enable = true;
  programs.pandoc.enable = true;
  programs.ripgrep.enable = true;
  programs.yt-dlp.enable = true;

  programs.wezterm = {
    enable = true;
    package = pkgsUnstable.wezterm;
    extraConfig = builtins.readFile ./conf/wezterm.lua;
  };

}
