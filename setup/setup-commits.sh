#!/bin/bash

DIR="${0%/*}"

echo -e "\e[1;34m}< ,.__)\e[0m \e[1m[commit hooks setup]\e[0m"

function install_npm_dependencies() {
    echo -e "\n\e[1m[npm]\e[0m"
    (cd ..; npm install 2>&1)
}

function install_husky() {
    echo -e "\n\e[1m[husky]\e[0m\n"
    (cd ..; npx husky install 2>&1)
}

install_npm_dependencies
install_husky

echo -e "\n\e[1;32m}< ,.__)\e[0m commit setup complete"
