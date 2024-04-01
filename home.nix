{ pkgs, pkgsUnstable, ... }:
with builtins;
with pkgs.lib;
with pkgs.stdenv;
let
  nu_scripts = pkgs.fetchFromGitHub {
    owner = "nushell";
    repo = "nu_scripts";
    rev = "707cda345078553f3e878a100ca103a28f440705";
    hash = "sha256-pgihFkuPIjFTLYtVKaXA+NPUfs/8TpWoojpGyi5TLhY=";
  };
in
{
  home-manager.users.${myUsername} = {
    home.stateVersion = "23.11";

    home.file.nushell-my-scripts = {
      source = ./conf/nushell;
      target = ".config/nushell/scripts/my";
      recursive = true;
    };

    home.file.nushell-contrib-custom-completions-scripts = {
      source = "${nu_scripts}/custom-completions";
      target = ".config/nushell/scripts/contrib/custom-completions";
      recursive = true;
    };

    home.file.nushell-contrib-nu-hooks-scripts = {
      source = "${nu_scripts}/nu-hooks";
      target = ".config/nushell/scripts/contrib/nu-hooks";
      recursive = true;
    };

    home.language.base = "en_GB.UTF-8";

    home.packages = with pkgs; [
      aws-vault
      fd
      mosh
      pkgsUnstable.nixd
      wget
      zstd
    ] ++ optionals hostPlatform.isDarwin [
      reattach-to-user-namespace
      shortcat
    ];

    home.shellAliases = {
      g = "git";
      ll = "ls -l";
      la = "ls -la";
    };

    programs.direnv = {
      enable = true;
      enableNushellIntegration = true;
      nix-direnv.enable = true;
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk;
    };

    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;

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
        credential = if hostPlatform.isDarwin then {} else {
          helper = "osxkeychain";
        };
        branch.sort = "committerdate";
        github.user = "dwb";
        init.defaultBranch = "main";
        pull.ff = "only";
        rebase.autoSquash = true;
        rerere.enabled = true;
      };
    };

    programs.helix = {
      enable = true;
      package = pkgsUnstable.helix;
    };

    programs.nushell = {
      enable = true;
      package = pkgsUnstable.nushellFull;
      configFile.text = concatLines (map readFile [
        ./conf/default_config.nu
        ./conf/config.nu
      ]);
      envFile.source = ./conf/env.nu;
    };

    programs.vim = {
      enable = true;
      defaultEditor = true;
      extraConfig = readFile ./conf/vimrc.vim;
    };

    programs.zsh = {
      enable = true;
      initExtra = readFile ./conf/zshrc.zsh;
    };

    programs.fzf.enable = true;
    programs.pandoc.enable = true;
    programs.ripgrep.enable = true;
    programs.yt-dlp.enable = true;

  };
}
