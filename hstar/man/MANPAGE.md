% hstar (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

hstar - An archiving tool

# DESCRIPTION

**hstar** is an archiving tool

# SYNOPSIS

  hstar sanitize archive.tar.lz

  hstar unpack src-dist-0.1.0.0.tar.zst

# SUBCOMMANDS

**unpack** - Unpack an archive

**pack-dir** - Pack a directory into an tarball

**pack** - Pack files into an archive

**pack-src** - Pack up a source directory, ignoring version control

**sanitize** - Convert a tarball to a pax-compatible archive. This reads the whole file into memory.

# OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

# SUPPORTED FORMATS

## COMPRESSION

  - gzip
  - bzip2
  - lzma
  - lzip
  - zstd
  - lz4
  - deflate
  - brotli (optional)

# SHELL COMPLETIONS

To get shell completions in your current session:

`eval "$(hstar --bash-completion-script hstar)"`

Put this in your `~/.bashrc` or `~/.bash_profile` to install them.

# BUGS

Please report any bugs you may come across to
http://github.com/vmchale/archive-backpack/issues.

# COPYRIGHT

Copyright 2020. Vanessa McHale. All Rights Reserved.
