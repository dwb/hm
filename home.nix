{ pkgs, pkgsUnstable, nixpkgs, nixpkgsUnstable, doomemacs, nu-scripts, username, ... }@args:
let
  inherit (pkgs) lib stdenv;
  guiEnabled = args.guiEnabled or stdenv.hostPlatform.isDarwin;
in
{
  imports = [
    ({ ... }@args: {
      config._module.args = {
        inherit guiEnabled doomemacs;
      };
    })
    ./emacs.nix
    ./nushell-vterm
    (import ./registry-pins.nix { inherit nixpkgs nixpkgsUnstable; })
    ./linkapps.nix
  ];

  # XXX: not in 24.05 branch yet
  # nix.channels = {
  #   inherit nixpkgs;
  # };

  home.stateVersion = "23.11"; # XXX: remember, don't change!

  home.username = username;
  home.homeDirectory = if stdenv.hostPlatform.isDarwin
                       then "/Users/${username}"
                       else "/home/${username}";

  home.language.base = "en_GB.UTF-8";

  home.packages = (with pkgs; [
    aws-vault
    fd
    git-absorb
    graphviz
    home-manager
    htop
    mosh
    nixd
    plantuml
    python3
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
  ]) ++ lib.optionals stdenv.hostPlatform.isDarwin (with pkgs; [
    reattach-to-user-namespace
  ]);

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

    userName = "Dani Brown";
    userEmail = "d@dani.cool";

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
      com = "checkout master";
      commend = "commit --amend --no-edit";
      di = "diff";
      dic = "diff --cached";
      dim = "!\"git diff $(git merge-base --fork-point master)\"..head";
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
        default = "upstream";
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
      url."ssh://git@github.com/".insteadOf = "https://github.com/";
    };
  };

  programs.helix = {
    enable = true;
    package = pkgsUnstable.helix;
  };

  programs.nushell = {
    enable = true;
    package = pkgsUnstable.nushell;
    configFile.text = lib.concatLines (
      ["source ${nu-scripts}/themes/nu-themes/windows-highcontrast-light.nu"] ++
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
    shortcut = "a";
    keyMode = "vi";
    newSession = true;
    mouse = true;
    clock24 = true;
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
    initExtra = lib.concatLines [
      (builtins.readFile ./conf/zshrc.zsh)
      (builtins.readFile ./conf/zshrc-local.zsh)
    ];
    envExtra = builtins.readFile ./conf/zshenv-local.zsh;
  };

  programs.fzf.enable = true;
  programs.pandoc.enable = true;
  programs.ripgrep.enable = true;
  programs.yt-dlp.enable = true;

}
