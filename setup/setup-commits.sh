#!/bin/bash

source "./variables.sh"

function install_npm_dependencies() {
    echo -e "\n${bold}[npm]${reset}"
    (cd ..; npm install 2>&1)
}

function install_husky() {
    echo -e "\n${bold}[husky]${reset}\n"
    (cd ..; npx husky install 2>&1)
}


echo -e "${blue}${whale}${reset} ${bold}[commit hooks setup]${reset}"

install_npm_dependencies
install_husky

echo -e "${green}${whale}${reset} commit setup complete"
