/* A Scheme reader compiler for Guile.
   Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

let
  buildOutOfSourceTree = true;

  jobs = {
    tarball =
      let
        pkgs = import <nixpkgs> {};
      in
        pkgs.releaseTools.sourceTarball {
          name = "guile-reader-tarball";
          src = <guile-reader>;

          preAutoconf =
            '' mkdir build-aux
               cp -v $(find ${pkgs.gettext} -name config.rpath) build-aux/
            '';

          buildInputs = with pkgs; [ gettext texinfo gperf guile pkgconfig ];
          dontBuild = false;
        };

    build =
      { system ? builtins.currentSystem }:

      let
        pkgs = import <nixpkgs> { inherit system; };
      in
        pkgs.releaseTools.nixBuild {
          name = "guile-reader";
          src = jobs.tarball;
          preConfigure =
            '' export configureFlags="$configureFlags --with-guilemoduledir=$out/share/guile/site/2.0"
            '';
          buildInputs = with pkgs; [ guile pkgconfig gperf ];
          inherit buildOutOfSourceTree;
        };

    build_lightning =
      { system ? builtins.currentSystem }:

      let
        pkgs = import <nixpkgs> { inherit system; };
      in
        pkgs.releaseTools.nixBuild {
          name = "guile-reader";
          src = jobs.tarball;
          preConfigure =
            '' export configureFlags="$configureFlags --with-guilemoduledir=$out/share/guile/site/2.0"
            '';
          buildInputs = with pkgs; [ guile pkgconfig gperf lightning ];
          inherit buildOutOfSourceTree;
        };

    build_guile18 =
      { system ? builtins.currentSystem }:

      let
        pkgs = import <nixpkgs> { inherit system; };
      in
        pkgs.releaseTools.nixBuild {
          name = "guile-reader";
          src = jobs.tarball;
          preConfigure =
            '' export configureFlags="$configureFlags --with-guilemoduledir=$out/share/guile/site"
            '';
          buildInputs = with pkgs; [ guile_1_8 gperf ];
          inherit buildOutOfSourceTree;
        };
  };
in
  jobs
