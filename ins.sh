#!/usr/bin/bash

dune build
sudo mv _build/default/bin/links.exe /usr/local/bin/links
