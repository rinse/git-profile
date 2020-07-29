# git-profile

Allows you to prepare profiles for git.

## installation

Use haskell-stack.

```bash
$ git clone git@github.com:rinse/git-profile.git
$ cd git-profile
$ stack install
```

## .gitprofile

Place a .gitprofile file on your home folder.
.gitprofile should be a yaml file like the following:

```yaml
profile1:
  user:
    name: name1
    email: name1@sample.com
  core:
    editor: vim

profile2:
  user:
    name: name2
    email: name2@sample.com
```

## Switch a profile

After placing your .gitprofile, you dive in any directories controlled
by git and switch a profile with the following command:

```bash
$ git profile switch profile1
```

It overrides your local git config.

```bash
$ cat .git/config
```

## Completion

Add the following to your .bashrc.

```bash
source <(git-profile --bash-completion-script `which git-profile`)

```

The following is also available when you prefer to zsh or fish.

* --zsh-completion-script: which is analogous for zsh;
* --fish-completion-script: which is analogous for fish shell;
