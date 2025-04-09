#!/usr/bin/env bash

set -e  # Exit immediately if any command fails

function install_cran_packages() {
  R -e 'install.packages(
      pkgs=c(
        "furrr"
      ), 
      repos="http://cran.us.r-project.org"
    )'
}

function install_github_package() {
  local pkg_repo=$1
  tokens=(${pkg_repo//_/ })

  local repo_name="${tokens[0]}"
  local pkg_name="${tokens[1]}"
  local pkg_ver="${tokens[2]}"

  local temp_repo_dir=$(mktemp -du)
  echo $temp_repo_dir
  git clone "https://github.com/${repo_name}/${pkg_name}.git" "${temp_repo_dir}"
  pushd "${temp_repo_dir}"
  R CMD build . 
  R CMD INSTALL "${pkg_name}_${pkg_ver}.tar.gz"
  popd
  rm -rf "${temp_repo_dir}"
}

install_cran_packages

install_github_package "ewarchul_cec2013_0.1-5"
install_github_package "ewarchul_cecs_0.2.5"
install_github_package "ewarchul_cecb_0.2.0"
