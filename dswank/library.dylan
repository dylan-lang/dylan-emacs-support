module:    dylan-user
author:    Andreas Bogk and Hannes Mehnert
copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library dswank
  use build-system;
  use commands;
  use common-dylan;
  use dfmc-back-end-implementations; // Is this used for effect?
  use environment-commands;
  use environment-internal-commands; // For side effect
  use environment-protocols;
  use file-source-records;
  use io;
  use lisp-reader;
  use network;
  use registry-projects;
  use release-info;
  use source-records;
  use system;
end library;

define module dswank
  // For modules from which we use very few names I'm using the full module name as a
  // prefix.  For the modules from which we use a lot of names, I use a much shorter
  // abbreviation. --cgay
  use build-system,
    prefix: "build-system/";
  use command-lines,
    prefix: "command-lines/";
  use commands,
    prefix: "commands/";
  use common-dylan;
  use environment-commands,
    prefix: "environment-commands/";
  use environment-internal-commands, // For side effect
    prefix: "environment-internal-commands/";
  use environment-protocols,
    prefix: "ep/";              // This is the big one.
  use file-system;
  use format;
  use lisp-reader;
  use locators;
  use registry-projects,
    prefix: "registry-projects/";
  use release-info,
    prefix: "release-info/";
  use sockets;
  use source-records,
    prefix: "sr/";
  use standard-io;
  use streams;
  use threads,
    import: { dynamic-bind };
end module;
