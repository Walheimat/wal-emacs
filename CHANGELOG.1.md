# Changelog

## [2.4.7](https://github.com/Walheimat/wal-emacs/compare/v2.4.6...v2.4.7) (2025-03-15)

### Configurations

* **casual-suite:** mothball package ([4599a8a](https://github.com/Walheimat/wal-emacs/commit/4599a8ac1fadf46e6ba0763ba3f9ab9c431ccf24))
* **display-line-numbers:** enable globally ([e29763e](https://github.com/Walheimat/wal-emacs/commit/e29763e9caac528eaa9fc00d851aebcc0197bf58))
* **display-line-numbers:** set type to visual ([8f441ff](https://github.com/Walheimat/wal-emacs/commit/8f441ffd65c3080a8f7e74790544b32b23b4eaf1))
* **js:** use js-ts-mode for mjs and cjs ([bce71b1](https://github.com/Walheimat/wal-emacs/commit/bce71b16b9d5b7cef28ea72ccc980853aceb1ce4))
* **org-roam-dailies:** create deadline entry with TODO ([10d0736](https://github.com/Walheimat/wal-emacs/commit/10d073687f924dc884b94a22f7122fd19fd6396d))
* **rg:** set new variable truncating search string in header ([44b182c](https://github.com/Walheimat/wal-emacs/commit/44b182ce54d4a1e8fc3ecedb32f17beb7abfa6ed))
* **settings:** set load-prefer-newer to t ([56c7c35](https://github.com/Walheimat/wal-emacs/commit/56c7c350db117642eda8884618f88631df627223))
* **ship-mate:** use prefix when reading commands ([8910972](https://github.com/Walheimat/wal-emacs/commit/8910972472622274ef4677d5afc846263a4402bd))

### Improvements

* **display-line-numbers:** no longer enable globally ([fa6bab5](https://github.com/Walheimat/wal-emacs/commit/fa6bab5310b3b016b0dc744aca4c83c72b919554))
* **java:** add compilation matcher for mvn test ([53265c2](https://github.com/Walheimat/wal-emacs/commit/53265c25a08666de07e2f489a9affc8a1c9047c8))
* **org-roam-dailies:** require after org-roam ([ff2fe83](https://github.com/Walheimat/wal-emacs/commit/ff2fe831c67c9e76d591ef40178ffba0149fdf03))
* **readme:** update and move version info to footnote ([1473729](https://github.com/Walheimat/wal-emacs/commit/14737297b8f3438840f053351bc6a02da83c5a60))
* **vterm:** differentiate into own command ([18d2b73](https://github.com/Walheimat/wal-emacs/commit/18d2b73595ce01e9e3af6559c7f830df8c558880))
* **vterm:** toggle line numbers when in copy mode ([2c34918](https://github.com/Walheimat/wal-emacs/commit/2c34918f91f8395acd42e173038723e71cc0cfc9))
* **wal-lsp-dwim:** signal error by default ([79ac404](https://github.com/Walheimat/wal-emacs/commit/79ac4047911d0c772d7ab0c356abc445ae4cc806))

### Other

* **avy:** M-j goes to char in line ([d91eac8](https://github.com/Walheimat/wal-emacs/commit/d91eac82c1e2dbe298e344e3f21a90b5f37f5d49))
* **dired:** bind dired-kill-subdir to M-i ([70c3d9e](https://github.com/Walheimat/wal-emacs/commit/70c3d9e7669ccfba62e888cd068e5edefc9b4f64))
* **display-line-numbers:** bind toggle in ambassador ([dcdd04d](https://github.com/Walheimat/wal-emacs/commit/dcdd04d1999ac15abd7db1640eef43aa4fe95888))
* **events:** move from <f5-8> to <f9-12> ([2d8bb1c](https://github.com/Walheimat/wal-emacs/commit/2d8bb1c3d5502d462b9cc879ff7901432e7fbccc))
* **lsp:** bind M-H-[] to lsp-execute-code-action ([0f83968](https://github.com/Walheimat/wal-emacs/commit/0f839680352c118a2a3bb7755f5c71c74d571ae3))
* **ship-mate:** bind quick-command-map ([a6a53b8](https://github.com/Walheimat/wal-emacs/commit/a6a53b8011fc9531775a09978aa0148ec8c10fa2))
* **tempel:** bind H-M-/ to tempel-complete ([a8022cb](https://github.com/Walheimat/wal-emacs/commit/a8022cb72711d7a151f69c4a1dd1a5a957931564))
* **vterm:** move to bare <f5> ([c78fb5e](https://github.com/Walheimat/wal-emacs/commit/c78fb5e68eaef89f1142d0ef1f8675178b0a4001))
* **wal-spill-paragraph:** move to C-M-q ([54691b6](https://github.com/Walheimat/wal-emacs/commit/54691b62fcab7e0cc1c076874bd5f6642f6d5a44))

## [2.4.6](https://github.com/Walheimat/wal-emacs/compare/v2.4.5...v2.4.6) (2024-10-27)

### Configurations

* **hungry-delete:** add package and use for lisp-mode ([f6bfb58](https://github.com/Walheimat/wal-emacs/commit/f6bfb584682515cb4bb315f7ee7154b41fd00ef9))
* **project:** add deno.json as a root marker ([70a0c87](https://github.com/Walheimat/wal-emacs/commit/70a0c874099189291cde10772be6008bc63e069f))
* **tab-bar:** set truncated-max to 8 ([c3499f9](https://github.com/Walheimat/wal-emacs/commit/c3499f99d2fd7534f8944c3c93fc30af5c314857))

### Bug Fixes

* **markdown-mode:** double-quote file ([8b1611f](https://github.com/Walheimat/wal-emacs/commit/8b1611fc69057b1d68437bb8487d536c3ef1af58))

### Improvements

* **consult,org-agenda:** narrow consult-buffer to agenda ([ebfb9d7](https://github.com/Walheimat/wal-emacs/commit/ebfb9d745c81ef4334f6ed8a5059b33c9676c979))
* **hungry-delete:** also activate for lisp-data-mode ([3933f3e](https://github.com/Walheimat/wal-emacs/commit/3933f3ec51b7949c8ecea0256592102ad80f198e))
* **jinx:** add predicate to exclude JavaScript exports ([311cdc1](https://github.com/Walheimat/wal-emacs/commit/311cdc17ace979a1a6c95c4488a0433cf8f665cb))
* **wal-supernova:** raise user-error when no target exists ([30697e9](https://github.com/Walheimat/wal-emacs/commit/30697e9c80c131d79b7e5367c2c859f05e0d1d4b))

### Other

* **ship-mate:** select command for alternate ([e704d2a](https://github.com/Walheimat/wal-emacs/commit/e704d2af5da9de70e3fc759ccdc7f9d7b553a792))
* **useful,consult:** user-prefixed backslash ([e07ca75](https://github.com/Walheimat/wal-emacs/commit/e07ca7517d6e22c1a257e2a766424e8f88015d48))
* **various:** use C for maps/transients, M for alternate keys ([a813761](https://github.com/Walheimat/wal-emacs/commit/a813761cbbcdc512520d450d5b5a5f5bb9d58f05))

## [2.4.5](https://github.com/Walheimat/wal-emacs/compare/v2.4.4...v2.4.5) (2024-10-13)

### Configurations

* **consult:** use preview key for consult-ripgrep ([349fd8d](https://github.com/Walheimat/wal-emacs/commit/349fd8d32101fc85007ff8c6357c912a0da04b6b))
* **drag-stuff:** re-add package ([dc756fa](https://github.com/Walheimat/wal-emacs/commit/dc756fafcacaecbb3bcba3331b18ee9485eba6a5))
* **ship-mate:** multiple buffers for execute ([1982c52](https://github.com/Walheimat/wal-emacs/commit/1982c52565a5bed5e6bf67a5a324de4883346ddb))
* **tab-bar:** use truncating name function ([cfd2926](https://github.com/Walheimat/wal-emacs/commit/cfd2926c61cedb5e7e787c7c3af534fff5874750))

### Bug Fixes

* **vertico:** use apply for no-cycle advice ([6cdfd44](https://github.com/Walheimat/wal-emacs/commit/6cdfd44ba6d55d20f216fea3c0c058ad3751d1d7))
* **visuals:** honor order in wal-preferred-fonts ([ea063b3](https://github.com/Walheimat/wal-emacs/commit/ea063b3731fd3f4daeadca1cb74816a17671c0b6))

### Improvements

* **config:** use less destructive char for whale nose ([51ea726](https://github.com/Walheimat/wal-emacs/commit/51ea726750650305a82ad118855c4d6e57e443b4))
* **consult:** don't cycle consult-{line,ripgrep} ([62d2e64](https://github.com/Walheimat/wal-emacs/commit/62d2e64bc48bac65598db7c0fad98d087c646238))
* **corfu:** no auto-completion for ship-mate-execute ([51b0e36](https://github.com/Walheimat/wal-emacs/commit/51b0e3664bb3f381e20153165a9ac97456163e77))
* **corfu:** no auto-completion in minibuffer ([af9e0c5](https://github.com/Walheimat/wal-emacs/commit/af9e0c580e26563c96d34bd752ff74262800863f))
* **lsp-mode:** remove formatting from wal-lsp-dwim ([ff337ba](https://github.com/Walheimat/wal-emacs/commit/ff337baafd1ec8ccb25c1f5cdc8edac2120514d6))
* **markdown-mode:** add function to open file with livedown ([6184925](https://github.com/Walheimat/wal-emacs/commit/6184925411195a0f4ea77a3149daf12fd14c493c))
* **org-clock:** use ellipsis when truncating heading ([4c1bf84](https://github.com/Walheimat/wal-emacs/commit/4c1bf8410f6c8e9b6519aa41105665f9f628de9a))
* **org-roam:** archive dailies in single file ([db044f1](https://github.com/Walheimat/wal-emacs/commit/db044f1b293785255c5244167c28950d8f19e127))
* **org-roam:** factor out dailies functionality ([9b552d8](https://github.com/Walheimat/wal-emacs/commit/9b552d812662889f66655b21c2b60073358b5c06))
* **org-roam:** single template when going to daily ([7562812](https://github.com/Walheimat/wal-emacs/commit/756281271e0125d2ebfa7bddf0206e9a0510966b))
* **org-super-agenda:** add group for achieved deadline ([3f075e5](https://github.com/Walheimat/wal-emacs/commit/3f075e5060c5ba937fb2742d7a5be0d883d854da))
* **org-super-agenda:** group of generally scheduled items ([7e68c85](https://github.com/Walheimat/wal-emacs/commit/7e68c85bff269734039ffb9edd64c18ca303cedc))
* **prettier:** advise on-save function to ignore prefix arg ([457b215](https://github.com/Walheimat/wal-emacs/commit/457b2155e33c40f4fe8a5e41252bc38e4c78e6af))
* **shell:** use new wal-quit-window-kill-buffer for dead shells ([52cc797](https://github.com/Walheimat/wal-emacs/commit/52cc797d306db9f25c98049c80a394fc6aad5574))
* **tempel:** use templates for more JS-based modes ([4b8a1f8](https://github.com/Walheimat/wal-emacs/commit/4b8a1f81aab25057c64a43c15725a8928e8ccdae))

### Other

* **corfu:** don't insert with RET ([554cf2a](https://github.com/Walheimat/wal-emacs/commit/554cf2a92ccb0e31db275a2a8746602ccd8246e5))
* **major:** uncomment major? command ([05d0dff](https://github.com/Walheimat/wal-emacs/commit/05d0dff5ff7257db508e6ba6c73730805f1c1291))

## [2.4.4](https://github.com/Walheimat/wal-emacs/compare/v2.4.3...v2.4.4) (2024-09-22)

### Configurations

* **clojure,cider,slime:** mothball packages ([f76e2de](https://github.com/Walheimat/wal-emacs/commit/f76e2de292276f9e76afb3c3f9806fa1b4932ad1))
* **consult-flycheck:** mothball package ([4137fe8](https://github.com/Walheimat/wal-emacs/commit/4137fe86b80e63fdeca796192262a62e6a4b0294))
* **display-line-numbers:** enable for prog(-like) modes ([f6823cf](https://github.com/Walheimat/wal-emacs/commit/f6823cf92c1abd757b7c544840b1a4547a1d0073))
* **eglot:** mothball configuration ([43ffc74](https://github.com/Walheimat/wal-emacs/commit/43ffc7467df9a093f8b37e068545971b894b06da))
* **email:** mothball section ([ceef0e6](https://github.com/Walheimat/wal-emacs/commit/ceef0e6f6f42bbde2533858800df65042f60a3f6))
* **flutter:** mothball section ([0b0ef05](https://github.com/Walheimat/wal-emacs/commit/0b0ef058cac1c1c06f6939e4c5c2600bc3de8ab4))
* **flycheck:** remove wal-flycheck-file ([d0de8f7](https://github.com/Walheimat/wal-emacs/commit/d0de8f7447a2e3ba5e34aa8049ea56767418a395))
* **flyspell:** mothball configuration ([c1f4201](https://github.com/Walheimat/wal-emacs/commit/c1f4201dd2def3accf3bc7c635e06ccdb1e63c72))
* **follow:** replace configuration with delight instruction ([c1215c9](https://github.com/Walheimat/wal-emacs/commit/c1215c9243ff3f5b425a0f0d414ce8c6c1af2488))
* **go:** mothball section ([99aed23](https://github.com/Walheimat/wal-emacs/commit/99aed2366096107c583f8e8a151e331d0144dc5c))
* **hideshow:** mothball config ([cd20791](https://github.com/Walheimat/wal-emacs/commit/cd20791d418d9754c24def45d8ef22a0797caaa8))
* **jinx:** lower-case, three-char lighter ([bb560cd](https://github.com/Walheimat/wal-emacs/commit/bb560cddd5b118a35bc770b26bc3df8a02074c10))
* **log4j-mode:** mothball package ([e5d7d9a](https://github.com/Walheimat/wal-emacs/commit/e5d7d9a8e1f8cff29b1bf2d8eb53cf9a4fcd792a))
* **org-roam-ui:** mothball package ([9abf9da](https://github.com/Walheimat/wal-emacs/commit/9abf9da6d665b0704eadcae0a1c0810bb09b2820))
* **org-roam:** two capture templates for dailies ([df357c4](https://github.com/Walheimat/wal-emacs/commit/df357c4c406bd7189edece6666e7109979a9d3b1))
* **php:** mothball section ([bd90890](https://github.com/Walheimat/wal-emacs/commit/bd90890be2de8729f606ef17ce9b1ca51f7022a9))
* **prolog:** mothball section ([b4420d1](https://github.com/Walheimat/wal-emacs/commit/b4420d16a84fbf4f80c2a81caaa2c3add3194ca3))
* **python:** mothball subpackages ([7d1dbe5](https://github.com/Walheimat/wal-emacs/commit/7d1dbe5f32462592d7443127b9be771c5593f54d))
* **python:** re-add pet and use it to set pyslp server command ([e8adff8](https://github.com/Walheimat/wal-emacs/commit/e8adff8428bcc09570a6719101f0e4c84aa3595d))
* **rg:** set buffer name to rg-results ([8d61014](https://github.com/Walheimat/wal-emacs/commit/8d61014272df4ee7be9afaf7459b0eee070a49a4))
* **subword-mode:** change lighter to something less confusing ([fbb977b](https://github.com/Walheimat/wal-emacs/commit/fbb977b6e470a0676a9d20c92b73452d6939982e))
* **text-mode:** don't use ispell capf ([c534444](https://github.com/Walheimat/wal-emacs/commit/c534444f865cffb7221a85d16861a6d11b5e94de))
* **visual-fill-column:** mothball package ([6571d48](https://github.com/Walheimat/wal-emacs/commit/6571d480781ead1aa60b6a08c017cb29e0e932b0))
* **web-mode:** mothball package ([2e8b329](https://github.com/Walheimat/wal-emacs/commit/2e8b329874e5e4cc77a23e34d47d65306b052e10))

### Bug Fixes

* **lsp-mode,corfu,orderless:** just set orderless as style ([213aca7](https://github.com/Walheimat/wal-emacs/commit/213aca7ed5c540520726df29e503424daa5580a6))

### Improvements

* **org-refile:** wal-org-refile can refile to default directory ([b6aac38](https://github.com/Walheimat/wal-emacs/commit/b6aac380a299e34cc588f5a1caff236e9f099895))
* **useful:** remove unused wal-async code ([4efe193](https://github.com/Walheimat/wal-emacs/commit/4efe19305b26f33516e144e499f53d485fa49788))

### Other

* **casual-suite:** bind new casual-agenda-tmenu ([a78b19d](https://github.com/Walheimat/wal-emacs/commit/a78b19d1d57c3f6baffef3cd6584f258279080f1))
* **completionist,cape:** swap map and cape-dabbrev ([50cfc6d](https://github.com/Walheimat/wal-emacs/commit/50cfc6da12d391e2f98eda2deefb07576bb4fa82))
* **dired-x:** add additional binding for dired-omit-mode ([730bf65](https://github.com/Walheimat/wal-emacs/commit/730bf6587826ffe21ccc968c12119db9a62b1534))
* **global:** prefer zap-up-to-char ([8c7b491](https://github.com/Walheimat/wal-emacs/commit/8c7b491fd8fe76ccea433a72d1b104571c1e7df3))
* **global:** unbind C-z (also bound to C-x C-z) ([cf8bbce](https://github.com/Walheimat/wal-emacs/commit/cf8bbcec3f07794b8534aea77ff5108cb0fa16d5))
* **org-agenda,consult:** move note-taking function ([61fa4ed](https://github.com/Walheimat/wal-emacs/commit/61fa4edf310b927d3159bb845a0d263420b294aa))
* **settings:** only use left-to-right text display ([7d03975](https://github.com/Walheimat/wal-emacs/commit/7d0397584611c2e515b890ed15f1bd3b0243e93e))
* **useful:** remove wal-mwim-beginning ([39f926f](https://github.com/Walheimat/wal-emacs/commit/39f926f348a1d522512103cdd77aa8b3f807bad7))

## [2.4.3](https://github.com/Walheimat/wal-emacs/compare/v2.4.2...v2.4.3) (2024-09-07)

### Configurations

* **casual-suite:** add and configure ([944c839](https://github.com/Walheimat/wal-emacs/commit/944c839aa427aa437901cd0b2e383d47d82c5c5a))
* **markdown-mode:** use LSP and enable prettier-mode ([3c68e80](https://github.com/Walheimat/wal-emacs/commit/3c68e8029d2f4d5b595ac156f37d6eccbf9ae0e0))
* **org-agenda:** use other-tab for setup ([b4de01d](https://github.com/Walheimat/wal-emacs/commit/b4de01d04013cc85979c3ee99f98aaad11f793b1))
* **org-super-agenda:** display items scheduled but with no time ([99a4689](https://github.com/Walheimat/wal-emacs/commit/99a46895bc2feb6a8d63fa283d7d2c170c986ce6))
* **org-super-agenda:** do collect non-todos ([0ffbc52](https://github.com/Walheimat/wal-emacs/commit/0ffbc526428f769032c4a95660f7ab3f2f2774b2))
* **org-super-agenda:** re-ogranize groups ([94f2f4d](https://github.com/Walheimat/wal-emacs/commit/94f2f4ddf8c5c13a07adde6673517e4d2317096e))
* **org-super-agenda:** today for dates, any todo for leftovers ([0f1b9d4](https://github.com/Walheimat/wal-emacs/commit/0f1b9d41be14ed5832d72aa79aa9243791a0c09e))

### Bug Fixes

* **bindings:** don't override wal-hyper-mock ([361c6f2](https://github.com/Walheimat/wal-emacs/commit/361c6f28953befe197ab3ed5ffd17a38f615ef63))
* **dap-mode:** only require subpackages after loading ([2d27935](https://github.com/Walheimat/wal-emacs/commit/2d279354cd20777f69783f08d5a5fb290a689f4e))
* **wal-lsp-dwim:** don't move to end of line for looking-at ([dd17e52](https://github.com/Walheimat/wal-emacs/commit/dd17e52a6f9eabf719316693cfe657f2174f7594))

### Improvements

* **consult:** add command to just display buffer ([63517f1](https://github.com/Walheimat/wal-emacs/commit/63517f103de911ff07b467802b7c6d7fc6919f85))
* **org-agenda:** rename created tab to "agenda" ([d26a5fe](https://github.com/Walheimat/wal-emacs/commit/d26a5fe21099171b8c1cef6bc9aebdb33e6fc80b))
* **org-clock:** allow setting any keyword on clock-out ([9726308](https://github.com/Walheimat/wal-emacs/commit/9726308fc41b0da7a000ba3feac76accd6bbb433))
* **org-super-agenda:** only use groups for org-agenda-list ([cf48e63](https://github.com/Walheimat/wal-emacs/commit/cf48e631ccb82498a6af409883d8b901c0a1ab76))
* **useful:** go to previous window after isearch-other-window ([dee6a8f](https://github.com/Walheimat/wal-emacs/commit/dee6a8f23b1bd41d52e92450b73395dfe2fd92c2))
* **wal-supernova:** only close first layer with argument ([9edda64](https://github.com/Walheimat/wal-emacs/commit/9edda64d08a9586159c89ccb9300a024d6954876))

### Other

* **bookmark:** bind bookmark-set also to b ([0893275](https://github.com/Walheimat/wal-emacs/commit/08932756c480ac0f06ef087b6f10ba346bac4f65))
* **ibuffer:** remap buffer-list to ibuffer-other-window ([2dc3838](https://github.com/Walheimat/wal-emacs/commit/2dc3838a62883dfd631412932f7b146fddf1a127))
* **org-agenda:** bind return to org-agenda-goto ([11432e3](https://github.com/Walheimat/wal-emacs/commit/11432e365d76a2b4ffe962cdfba83c3d405d077e))
* **org-roam:** make capturing the default binding again ([1aeaf2f](https://github.com/Walheimat/wal-emacs/commit/1aeaf2f65ff65d084e5c1eff688274a1a99e840f))

## [2.4.2](https://github.com/Walheimat/wal-emacs/compare/v2.4.1...v2.4.2) (2024-08-24)

### Configurations

* **ace-window:** always dispatch ([2ed6bae](https://github.com/Walheimat/wal-emacs/commit/2ed6baeed11a1b3e0a0c5ff14326e414f23aaac3))
* **ace-windows:** don't always dispatch but repeat ([fe82e01](https://github.com/Walheimat/wal-emacs/commit/fe82e01962d6cfcfc103cc9955df0475decc0454))
* **gumshoe:** mothball package ([b3639d0](https://github.com/Walheimat/wal-emacs/commit/b3639d047defcf4f09bc57b0e33c5befb50fc47b))
* **markdown-mode:** don't enable auto-fill-mode ([bdceef5](https://github.com/Walheimat/wal-emacs/commit/bdceef5e9b5221f28da6a207f2585911b03981f7))
* **org-mode:** don't enable auto-fill-mode ([e4b80d5](https://github.com/Walheimat/wal-emacs/commit/e4b80d57fd904c3ddbdb7ba0d82afc2748c1843e))
* **partial-recall:** set narrow key to i ([3ed762a](https://github.com/Walheimat/wal-emacs/commit/3ed762a1ec6b8084615421d75aa28edd1695b231))
* **settings:** no automatic rehydration ([d2a63ce](https://github.com/Walheimat/wal-emacs/commit/d2a63ce03240c21ee3c2c4c2a6103e1a0256ec7a))
* **text-mode:** enable visual-line-mode ([e3006e2](https://github.com/Walheimat/wal-emacs/commit/e3006e2b44ec322d23539b1fb71fe95af25b67cf))
* **transpose-frame:** flop frame, command map with meta ([3a82b23](https://github.com/Walheimat/wal-emacs/commit/3a82b23946aba058ec7ef074e83d917cf3fda1ae))
* **zig-mode:** add and configure ([2744e97](https://github.com/Walheimat/wal-emacs/commit/2744e9764212abe7dcbd4b66255bed39252067e8))

### Bug Fixes

* **lang,harpoon:** call macro in init step ([5a67afb](https://github.com/Walheimat/wal-emacs/commit/5a67afbd7e1fa2384bc96ff54dd8ffb95c256db4))
* **rg:** also toggle context flag when there are no flags ([f9ca33f](https://github.com/Walheimat/wal-emacs/commit/f9ca33f741bcb5837f14d5d7d847c008684a5b04))

### Improvements

* **consult,vertico:** use flat display for consult-buffer ([3d41218](https://github.com/Walheimat/wal-emacs/commit/3d41218b1fbd10d60f83393ff88af954a4e1ca1d))
* **consult:** bind normal and symbol-at-point variant of -line ([2e37e03](https://github.com/Walheimat/wal-emacs/commit/2e37e03db1cbbd826b31c2d06468cbc7bbac3538))
* **consult:** require explicit preview for consult-buffer ([0b131b0](https://github.com/Walheimat/wal-emacs/commit/0b131b0955c91b4d4f2f537f7423db14867fd7dd))
* **lsp:** jump to lens, find references as part of dwim ([7436641](https://github.com/Walheimat/wal-emacs/commit/7436641329985091f3a0f4ec5841a0a97ec52b8e))
* **project:** add command to find dir-locals ([040c11d](https://github.com/Walheimat/wal-emacs/commit/040c11dfe11df346321552fa1257a15d05c3d5a6))
* **register:** make storing window configuration the alternative ([f281045](https://github.com/Walheimat/wal-emacs/commit/f2810455da4cdeacea2c5e207828050c2092e7bb))
* **text-mode:** enable electric-pair-mode instead ([d310c74](https://github.com/Walheimat/wal-emacs/commit/d310c74619e7fb8c77a5eaa566b2633d05087d62))
* **useful:** find symbol's bounds ([3bff4b3](https://github.com/Walheimat/wal-emacs/commit/3bff4b3e966e325cf5ed093e3b8870ad77f7656d))
* **various:** add package tags describing usage volume ([aa58a6b](https://github.com/Walheimat/wal-emacs/commit/aa58a6b1c117e944f903ccfb9a56051d8da86502))
* **vertico:** use flat when using ace-window ([5dcc908](https://github.com/Walheimat/wal-emacs/commit/5dcc9083dc8e1bf5a3091eb8c03b6e7d03c9a684))
* **visual-fill-column:** don't enable after visual-line-mode ([e7db43c](https://github.com/Walheimat/wal-emacs/commit/e7db43c8e230012b4a0644352b53523e922a8807))
* **yaml-mode:** set standard-indent from custom variable ([b1427b2](https://github.com/Walheimat/wal-emacs/commit/b1427b20559c8fdb0d700e9c91a0ef792ef14853))

### Other

* **ace-window,tab-bar:** fold into o ([d56266e](https://github.com/Walheimat/wal-emacs/commit/d56266e83464dc15d2a1f68844519310120db266))
* **ace-window:** move to o, bind other-window-* in C-c o ([f197f39](https://github.com/Walheimat/wal-emacs/commit/f197f39ce7e9984ab0a63d574410ff7abd51d946))
* **ace-window:** use u for consult in dispatch ([5fce007](https://github.com/Walheimat/wal-emacs/commit/5fce007aaa8a3450583b1041c368bd9a9b6d0585))
* **cape:** cape-dabbrev to user-prefixed M-/ ([ae8d96b](https://github.com/Walheimat/wal-emacs/commit/ae8d96b7c43f153d1126deb2157f58148afdbe6c))
* **compile:** fold {re-}compile into C-c r ([7f769e1](https://github.com/Walheimat/wal-emacs/commit/7f769e1a2161f975dd32a6258cdddecac6e21380))
* **complete:** move goto-mark back to transient ([545216d](https://github.com/Walheimat/wal-emacs/commit/545216d5ddd12cfe25bd51f2f0d65dce478bafa4))
* **consult,dap,lsp,roam:** move to better rows ([2cfe2a1](https://github.com/Walheimat/wal-emacs/commit/2cfe2a15f7554d0ad85ff56a256b4ffb7b1b7571))
* **consult,partial-recall:** ,=>u and u=>i ([b2bcf74](https://github.com/Walheimat/wal-emacs/commit/b2bcf74049bfbfb7f86d4f512e8af11385750e9a))
* **consult:** bind ' to consult-mark ([572b708](https://github.com/Walheimat/wal-emacs/commit/572b708e4730bb513aeabbfff3ed3f9ce4377362))
* **consult:** bind mark commands to ' ([8efa32b](https://github.com/Walheimat/wal-emacs/commit/8efa32b59bce1c948614cf24addec3c91426ce73))
* **consult:** switch to user-prefixed k for previews ([144046b](https://github.com/Walheimat/wal-emacs/commit/144046b0a3adf3628ca5f2ac8a7c7c526242a619))
* **eww,outline,bookmark:** shuffle bindings ([8061000](https://github.com/Walheimat/wal-emacs/commit/8061000d7ec12e2f36c5340500df1e63148245ec))
* **marginalia,embark:** make user-prefixed ; the cycle key ([023ecc2](https://github.com/Walheimat/wal-emacs/commit/023ecc2b67aadbbd095d8424f55aa1e5dd5c6080))
* **org-agenda:** fold into C-c a ([8b83e4b](https://github.com/Walheimat/wal-emacs/commit/8b83e4b31be8015078c7355b072299d11ba6f912))
* **org-capture:** fold into C-c c ([677bb44](https://github.com/Walheimat/wal-emacs/commit/677bb44b3c36daf47e8de100ef5c451afbb40891))
* **org-roam:** make node-find the default binding ([845087d](https://github.com/Walheimat/wal-emacs/commit/845087da1060c4c9a20dce32ab1b8c338092f68b))
* **org-roam:** move to p ([b9d84f9](https://github.com/Walheimat/wal-emacs/commit/b9d84f976636c7a847c06320db35d3102bb7a57e))
* **org,dap,lsp:** revert to previous bindings ([46b802d](https://github.com/Walheimat/wal-emacs/commit/46b802d89284b2c72e7d4d29258c8556f03eec2e))
* **project,register:** switch,find,jump => ',h,y ([61ad645](https://github.com/Walheimat/wal-emacs/commit/61ad645eb07af01643a2660a5cae6b4911c52c4e))
* **project,ship-mate:** use prefixes to p and , ([0c4ff89](https://github.com/Walheimat/wal-emacs/commit/0c4ff898effd29e1bc283713695b20f2f615e42c))
* **project:** move from h=>y ([182468a](https://github.com/Walheimat/wal-emacs/commit/182468acc3aedc05a07fa1688e56ffa87b33bc42))
* **project:** swap project-{find-file,switch} ([3dc7b72](https://github.com/Walheimat/wal-emacs/commit/3dc7b725f3f6f2614f9499f7eb15119617cbeae8))
* **register,consult:** swap jump and search ([7ec5890](https://github.com/Walheimat/wal-emacs/commit/7ec5890b36181a813ba0c0568003a4b2976af534))
* **useful:** move wal-supernova to adjunct ([279081d](https://github.com/Walheimat/wal-emacs/commit/279081dbf4c78ad71add8c6d5468b52645bf1b62))
* **various:** move all C-c bindings to the left hand ([4299679](https://github.com/Walheimat/wal-emacs/commit/4299679de285231564657cc6f6f1ada5fcb1614e))

## [2.4.1](https://github.com/Walheimat/wal-emacs/compare/v2.4.0...v2.4.1) (2024-08-11)


### Configurations

* **ace-window:** add delete-other-windows to dispatch ([3a4cb00](https://github.com/Walheimat/wal-emacs/commit/3a4cb00ef0a593085f424745cf1c5f6f2089ae25))
* **ace-window:** dispatch when more than 1 window ([d7a382c](https://github.com/Walheimat/wal-emacs/commit/d7a382c5fa0812401c906d4b90a0f6cf9f9c6453))
* **adjunct:** mimic window-prefix-map of Emacs 30 ([0580b8c](https://github.com/Walheimat/wal-emacs/commit/0580b8c88bc7fb9e7691ecf194c6860acd63298e))
* **adjunct:** move swipe-window-prefix to user prefixed M-o ([a1d06d5](https://github.com/Walheimat/wal-emacs/commit/a1d06d5e6d575c3455023c17f84e8e602c2a1f74))
* **avy:** use at-full and closest for goto-line ([92db43c](https://github.com/Walheimat/wal-emacs/commit/92db43c9eff942c3df311f06144ac9b12ee5d41c))
* **completionist:** re-bind to user-prefixed forward slash ([c6e857d](https://github.com/Walheimat/wal-emacs/commit/c6e857d4caff8c48a868ad1add66dc9a2a1285aa))
* **corfu:** C-j inserts ([eaf7474](https://github.com/Walheimat/wal-emacs/commit/eaf74741063e0932d5f3233245140c92bc97a517))
* **flymake:** fix indicator-type to fringes ([2b67ad4](https://github.com/Walheimat/wal-emacs/commit/2b67ad45196a0d5e617f774fcb32e1c02df53b99))
* **hideshow:** enable in prog(-like) modes, re-bind keys ([cadd5dd](https://github.com/Walheimat/wal-emacs/commit/cadd5dd26eb9686c3440905af135a7db617ddec7))
* **lsp-sonarlint:** update configuration ([b94faf4](https://github.com/Walheimat/wal-emacs/commit/b94faf440f19fd7c34097f0e77a2d0b07ff0e40a))
* **partial-recall:** enable new hygiene behavior ([833a09c](https://github.com/Walheimat/wal-emacs/commit/833a09cf1ffa981d7ca4865f9494eab6591d6919))
* **repeat:** set repeat-exit-key to q ([c542ea6](https://github.com/Walheimat/wal-emacs/commit/c542ea6638828fe0466366810c4f88c7b43d658a))
* **surround:** remove broken general keymap binding ([2108335](https://github.com/Walheimat/wal-emacs/commit/2108335560036fde47913656dfd66f59c8b70228))
* **various:** rebind user-prefixed k, i and o ([bda5708](https://github.com/Walheimat/wal-emacs/commit/bda5708914b57d2070bd5d4154702cfd8ff43b66))


### Bug Fixes

* **find-project-tasks-heading:** don't set override to nil ([2b3e53a](https://github.com/Walheimat/wal-emacs/commit/2b3e53aac1255861c7c0ce9a1296674d6c2e0ca4))


### Improvements

* **ace-window:** add partial-recall buffer switch to dispatch ([5f01fe0](https://github.com/Walheimat/wal-emacs/commit/5f01fe01495fede5beb7cceac779edc5f52034c4))
* **ace-window:** use common macro ([3a7c312](https://github.com/Walheimat/wal-emacs/commit/3a7c3123101b187a5bf5570e6a8ec7e9a063a1bf))
* **avy:** order goto-line using line number ([addad3e](https://github.com/Walheimat/wal-emacs/commit/addad3e3e060a7d4943c1a7638d8b2d2bdfe3cd9))
* **config,org-agenda:** package tag search as custom command ([4c436e7](https://github.com/Walheimat/wal-emacs/commit/4c436e73b402aaf86319ae308121a21bae26f785))
* **config:** re-order sections and code ([1eeeaa4](https://github.com/Walheimat/wal-emacs/commit/1eeeaa41ebced6f8fea7844b863a6ba31616d1df))
* **consult:** hide buffers of agenda source ([1a81b42](https://github.com/Walheimat/wal-emacs/commit/1a81b425a1b0ab915208b8616cdbdfb9d2d53c42))
* **dap-mode:** create custom global minor mode with bindings ([7b8a9b3](https://github.com/Walheimat/wal-emacs/commit/7b8a9b3f2ca178fbac78f183011e984fb769df43))
* **prelude:** allow setting Emacs version used ([001d9c9](https://github.com/Walheimat/wal-emacs/commit/001d9c92547d824dbaf4e506883aa2d420ebed73))
* **transpose-frame:** extend on explanation why it is useful ([f9ff479](https://github.com/Walheimat/wal-emacs/commit/f9ff479736bdfeb634c5c030e27cfe9d3a28b6ab))

## [2.4.0](https://github.com/Walheimat/wal-emacs/compare/v2.3.1...v2.4.0) (2024-06-23)


### Features

* **settings:** add wal-other-window-for-scrolling ([b88758e](https://github.com/Walheimat/wal-emacs/commit/b88758e3482741d13cafa8aa5092c3ce5854dd3f))
* **transpose-frame:** add package, create keymap ([a108423](https://github.com/Walheimat/wal-emacs/commit/a108423ed2ad0ab3efc6858b30bef76cbf6c45db))
* **useful:** wal-swipe-window-prefix ([f6146a9](https://github.com/Walheimat/wal-emacs/commit/f6146a982537181e733dca8effcf783ce164c345))
* **wal-other-window-mru:** add and bind to M-o ([8920a18](https://github.com/Walheimat/wal-emacs/commit/8920a18ae10ba668ed01ebfd451aeb3a0a266ed6))


### Configurations

* **avy:** invert keys of left hand ([db5b41f](https://github.com/Walheimat/wal-emacs/commit/db5b41f273f039eb1df77ae3231a581b326a4e6d))
* **avy:** use style at for word-0 ([dfcfd40](https://github.com/Walheimat/wal-emacs/commit/dfcfd401f6495260855510e36a96429081576f81))
* **avy:** use words style everywhere ([8fd8976](https://github.com/Walheimat/wal-emacs/commit/8fd8976232a7d2e0c6eb0c9a396a3a8f36f1290d))
* **avy:** word-0 at-full, order-closest; re-order keys ([362851c](https://github.com/Walheimat/wal-emacs/commit/362851cf31380857f7ba590dcc27ab85b4a8b1e2))
* **cape,tempel:** use common command map bound to C-c k ([9d966bd](https://github.com/Walheimat/wal-emacs/commit/9d966bdc71a6aaf7ed0c26300e90cf6fe303f6b9))
* **consult,avy:** bind H-M-l to consult-goto-line ([a309ca8](https://github.com/Walheimat/wal-emacs/commit/a309ca8415bfc716c70fd88f9ffa1fe150c4b281))
* **consult,project:** simplify bindings ([f14abee](https://github.com/Walheimat/wal-emacs/commit/f14abee726858c34a7132b29eed2975ed5fb67e1))
* **consult:** hide compilation buffer source ([aa9ccdc](https://github.com/Walheimat/wal-emacs/commit/aa9ccdcb628dec2e4a8fba8f42c570c47878961f))
* **consult:** single quote switches projects ([166feed](https://github.com/Walheimat/wal-emacs/commit/166feed4cd4751754b788f12941f8a34de67c2ff))
* **corfu-quick:** match keys to vertico-quick ([0ecaf01](https://github.com/Walheimat/wal-emacs/commit/0ecaf01c389f2653af2f35081beaa29722b4758c))
* **corfu,embark:** completion to k, embark to single quote ([149236e](https://github.com/Walheimat/wal-emacs/commit/149236e81720097e3d4088a5c18597fe7b5a44f9))
* **corfu:** bind completionist to user-prefixed M-k ([d4fe7ac](https://github.com/Walheimat/wal-emacs/commit/d4fe7ac88fc7e91ee7aec5b3be323d1bd00c758e))
* **corfu:** increase auto-delay, bind user-prefixed . to cap ([1020115](https://github.com/Walheimat/wal-emacs/commit/10201158e233ccb567e7b60bfadca3474c83df55))
* **corfu:** remove additional binding for corfu-insert-separator ([a1ca633](https://github.com/Walheimat/wal-emacs/commit/a1ca6334df5287b47e77c4644339f7cd3930e45a))
* **corfu:** use default auto delay ([1790f2d](https://github.com/Walheimat/wal-emacs/commit/1790f2d60ce614b96d4a969a3157e23909f81136))
* **dap-mode:** bind plain to dap-next, meta to command map ([6afa202](https://github.com/Walheimat/wal-emacs/commit/6afa202762f3509f22eea9f19b6dc2954e327a67))
* **dired-x:** disable dired-omit-verbose ([775144c](https://github.com/Walheimat/wal-emacs/commit/775144c948a485870c215a06ec1ec2e1ad514d0e))
* **dired-x:** run dired-omit-mode for dired ([c1bd846](https://github.com/Walheimat/wal-emacs/commit/c1bd846296ef8f72c0c0e59ee40e30a03e66efa9))
* **dired:** don't remove j binding in repeat-map ([9b825a7](https://github.com/Walheimat/wal-emacs/commit/9b825a776a5b244aa02d82c989034eaf12128092))
* **dumb-jump:** add additional project markers ([040390a](https://github.com/Walheimat/wal-emacs/commit/040390a8ef6ba7e5abe7eb056baf707e8d1faa0b))
* **dumb-jump:** move wal-dumb-jump-go back to editor ([11f84b3](https://github.com/Walheimat/wal-emacs/commit/11f84b3435806651de208bb4fc01da4883f37582))
* **embark:** move to user-prefixed period ([12af10d](https://github.com/Walheimat/wal-emacs/commit/12af10d4a7f7119eca6a7e1e46c3e7cbf1b8c6b4))
* **gd-script,emacs-lisp-mode:** remove custom completion settings ([b2cae51](https://github.com/Walheimat/wal-emacs/commit/b2cae51f4187b182fc269ae26f0261c9b4c1ff4c))
* **gdscript-mode:** use default indent offset ([a442f27](https://github.com/Walheimat/wal-emacs/commit/a442f27095e628f6f04299fcc1fbb1b3a4967a8e))
* **gdscript-mode:** use lsp-mode ([25b5ce0](https://github.com/Walheimat/wal-emacs/commit/25b5ce0864dc7ffddbeaf94c6839f7afb9b42420))
* **global:** bind modifiers to function keys ([5658063](https://github.com/Walheimat/wal-emacs/commit/56580630dd2e68032d105c73dde4650fc79dd745))
* **key-bindings:** remove simulations of C-{x,c} ([414eebf](https://github.com/Walheimat/wal-emacs/commit/414eebf81a44b3b6974f933dd56375e9131c5cc5))
* **lsp-mode,treemacs:** don't set no-delete-other-windows ([497d654](https://github.com/Walheimat/wal-emacs/commit/497d654944502a57edf20b67dd5a749bdd5410ad))
* **lsp-mode:** set force-aligned for HTML LSP ([fe7e0d3](https://github.com/Walheimat/wal-emacs/commit/fe7e0d310f2654044fe7c68de130204bbf849a5a))
* **lsp-ui:** enable sideline ([96fe205](https://github.com/Walheimat/wal-emacs/commit/96fe205fc6171e651d678793dda046da9f225526))
* **magit-process:** apply ANSI colors ([717d1c9](https://github.com/Walheimat/wal-emacs/commit/717d1c94e60b0709a5b72ce5f49a7124de0aecd7))
* **magit-process:** increase popup time ([02177fa](https://github.com/Walheimat/wal-emacs/commit/02177fa8a94d4e3fc5a340b5f65d83032a49cb91))
* **magit-process:** set popup time to 4 seconds ([0af383a](https://github.com/Walheimat/wal-emacs/commit/0af383a8853cf80df8792334e3c2b7b85c5edeb5))
* **org,markdown-mode:** enable auto-fill-mode ([fd42eb6](https://github.com/Walheimat/wal-emacs/commit/fd42eb6d3041c74dd2e69e2cfea5e98e053e9778))
* **org:** enable enforce-todo{-checkbox}-dependencies ([880a562](https://github.com/Walheimat/wal-emacs/commit/880a5622f88a08c711bdb05a1837cba9a5b16ffd))
* **package:** bind find-in-here to user-prefixed M-h ([b381dbe](https://github.com/Walheimat/wal-emacs/commit/b381dbed562295322934c4f1e85ff5d2c8ec6483))
* **project:** bind remember-projects-under to m in map ([46090f8](https://github.com/Walheimat/wal-emacs/commit/46090f8d339a2597355002d6a53a77f0d2f193b6))
* **project:** switch-to-parent-project => meta quote ([0c75d8f](https://github.com/Walheimat/wal-emacs/commit/0c75d8f2230c88dc9a117fcee3c91588951e3a24))
* **project:** wal-project-find-in-here in project-prefix-map ([20c4ad2](https://github.com/Walheimat/wal-emacs/commit/20c4ad2e4c353db3fc77be39e79e8e4a485fb6b3))
* **puni:** move around, no caps ([7dd2e90](https://github.com/Walheimat/wal-emacs/commit/7dd2e9099d23a7a601add862aa7f6fea27bf9a1e))
* **register:** bind quick register to user-prefixed period ([5d64b4b](https://github.com/Walheimat/wal-emacs/commit/5d64b4b9e7009ded78d1d36883d400a2fb8ad530))
* **register:** don't bind window-configuration-to-register ([091f9f2](https://github.com/Walheimat/wal-emacs/commit/091f9f242c23c404a141e94f7bd6d49adeda93b8))
* **register:** remove quick register ([cc8f3cc](https://github.com/Walheimat/wal-emacs/commit/cc8f3ccf7a89e191b89af618e9c0a42581dae50a))
* **rg:** use ripgrep for buffer name ([4a537e2](https://github.com/Walheimat/wal-emacs/commit/4a537e24c06043a34d6d088de83d418e895bbf6c))
* **rust-mode:** bind debug command ([78ce7c0](https://github.com/Walheimat/wal-emacs/commit/78ce7c00d796519a6078dfa639ebda3660a49935))
* **rust-mode:** enable new harpoon feature to format before save ([3fdd648](https://github.com/Walheimat/wal-emacs/commit/3fdd64810cf8814e2aed716b09532cb51158e131))
* **rust-mode:** load dap-cpptools for debugging ([257dfca](https://github.com/Walheimat/wal-emacs/commit/257dfcae439b6f227f30ed0a6f37c2156ee95859))
* **ship-mate:** use new keyword for execute commands ([2aa64fa](https://github.com/Walheimat/wal-emacs/commit/2aa64fa95adbcc346045b043a4607627f2a5fcb4))


### Bug Fixes

* **avy:** require avy for custom commands ([6db5d7d](https://github.com/Walheimat/wal-emacs/commit/6db5d7d19b9d24c96c92e829c9505ebce0995162))
* **org:** don't hardcode in-progress state string ([266005b](https://github.com/Walheimat/wal-emacs/commit/266005b23b00d3e99e5b1ec72ae879949af7fadd))
* **text-mode:** don't globally set auto-fill-mode ([2d936e6](https://github.com/Walheimat/wal-emacs/commit/2d936e6c756d9ab359642072bf0f80377cee423c))
* **wal-doppelganger:** don't display indirect buffer ([0c5c621](https://github.com/Walheimat/wal-emacs/commit/0c5c62120d5c91d24f4cb7bb457f53267618c03f))
* **workspace:** make finding in directory work with embark ([dcec633](https://github.com/Walheimat/wal-emacs/commit/dcec6337a38f73778a7d4ac0a8cde045a0e48f36))


### Improvements

* **ace-window:** re-add minimal configuration ([fd67f40](https://github.com/Walheimat/wal-emacs/commit/fd67f40480119bf0d7f05e8f4652e57d039777b0))
* **ace-window:** simplify dispatch alist ([f942f98](https://github.com/Walheimat/wal-emacs/commit/f942f9809ff3e7eea293b5b00eec9d809e806e51))
* **avy:** only use at-full in custom word-0 ([79b2ac3](https://github.com/Walheimat/wal-emacs/commit/79b2ac331c8ee927029d42ecac43636e03fab638))
* **bindings:** add which-key replacements for new key ([58ec7dd](https://github.com/Walheimat/wal-emacs/commit/58ec7dd635a5e078484f6181ce46dbedfadbba7a))
* **dired-x:** add unnumbered property ([3ca2f32](https://github.com/Walheimat/wal-emacs/commit/3ca2f32dfa554685ff58ad7a20555cf06c94372f))
* **dumb-jump:** call interactively and set to not prompt ([66543b7](https://github.com/Walheimat/wal-emacs/commit/66543b753571a3c8eda2ac28d794523273fd136d))
* **lsp-mode:** mention why `lsp-enable-snippet` is disabled ([fc5fbc7](https://github.com/Walheimat/wal-emacs/commit/fc5fbc7e3beae03773a8c7a43b793f02ddf78b45))
* **register:** select window on jump to marker ([c4bd738](https://github.com/Walheimat/wal-emacs/commit/c4bd7388873223b7838eb4c8f27017e454e53107))
* **tempel:** add ert template ([b03e3a1](https://github.com/Walheimat/wal-emacs/commit/b03e3a193b6ed463d8efbaa92abbb0ffb2547473))
* **useful:** add wal-doppelganger ([6bfb396](https://github.com/Walheimat/wal-emacs/commit/6bfb39693fbddbeafeb3d8ad894519cac3789aa3))
* **useful:** wal-isearch-other-windows ([b6e7985](https://github.com/Walheimat/wal-emacs/commit/b6e79854890fbd30452e98cfccfa8e14f268ae77))
* **useful:** wal-other-window ([965d800](https://github.com/Walheimat/wal-emacs/commit/965d8009e795fec2261cd9c33ffbe173f6471e6d))
* **vterm:** disable query-on-exit flag ([659be4d](https://github.com/Walheimat/wal-emacs/commit/659be4d6a77f9a77a5e0eed25f162bb68bb31f1a))
* **wal-lsp-dwim:** don't format buffer at line-end ([fc88104](https://github.com/Walheimat/wal-emacs/commit/fc8810407b5dea32efb79afd7e9649da11e8fd50))

## [2.3.1](https://github.com/Walheimat/wal-emacs/compare/v2.3.0...v2.3.1) (2024-03-24)


### Configurations

* **cape:** bind to user-prefixed forward slash ([2d5210d](https://github.com/Walheimat/wal-emacs/commit/2d5210dcc6d4dae9fe79fcd8424fc9727406e8e8))
* **dap-mode:** bind delete-all, re-bind breakpoints-list ([4b7cf6c](https://github.com/Walheimat/wal-emacs/commit/4b7cf6c9414d982d4e697914e3d007303b26383e))
* **dumb-jump:** bind plain dumb-jump to user-prefixed period ([c1c5650](https://github.com/Walheimat/wal-emacs/commit/c1c565068ae3c1275d123dee1a5d5f25e79c0df7))
* **general,config:** move whaler to f5, remove as leader ([0a5775f](https://github.com/Walheimat/wal-emacs/commit/0a5775feaf518b8ee8c05f7ae8f3f7a476083d26))
* **general:** bind all in number row ([d63b79c](https://github.com/Walheimat/wal-emacs/commit/d63b79cc0d693195b0077f157abec85956a3750e))
* **general:** move administrator to f6 and remove as leader ([4d621a9](https://github.com/Walheimat/wal-emacs/commit/4d621a952140f1552896a63633468377facc782b))
* **general:** zero-in=>seeker, triple-minus=>adjunct ([9496919](https://github.com/Walheimat/wal-emacs/commit/94969190615123baae1328e842e5f107d989fe20))
* **key-bindings:** bind package-upgrade ([5a8657a](https://github.com/Walheimat/wal-emacs/commit/5a8657a16810c4d1edca9e0806428546228a1147))
* **leaders:** re-assign, only blacklist transient leaders ([22d136b](https://github.com/Walheimat/wal-emacs/commit/22d136bfcdd9265d2fb8bcba9eb8fc3d6d530cf0))
* **lsp-mode:** bind command map to meta backslash ([c92b621](https://github.com/Walheimat/wal-emacs/commit/c92b62108ce622023a1e0519574a93017f12b4e8))
* **lsp-mode:** move to user-prefixed period ([ec493ec](https://github.com/Walheimat/wal-emacs/commit/ec493eccdeec79c28571c10748dbb8fdca32d896))
* **lsp, dap:** bind to user-prefixed brackets ([526bd15](https://github.com/Walheimat/wal-emacs/commit/526bd159258ead8ddab8c103a83bbd523a894695))
* **magit:** don't restore window config, use default ([94fb908](https://github.com/Walheimat/wal-emacs/commit/94fb9083091e47cb5c0ae6395a7ba41c5c447dcf))
* **org-roam:** bind capture to user-prefixed quote, menu to meta ([8b846a6](https://github.com/Walheimat/wal-emacs/commit/8b846a6b1ecf44b8ead15a48d5188e1d2d6d1f90))
* **project, org-roam:** project uses quote, roam backslash ([a7c68c0](https://github.com/Walheimat/wal-emacs/commit/a7c68c09d8b0b6a4af974fc5110868d0f3228d19))
* **rust-mode:** remove cargo-mode, bind cargo-process commands ([e13188a](https://github.com/Walheimat/wal-emacs/commit/e13188a847adab3b08ee8da92876802e4c10d9e1))
* **simple:** add alt bindings of undo{-redo} to repeat map ([1e98226](https://github.com/Walheimat/wal-emacs/commit/1e9822692881b8cbac06e75e847b5f1db4294696))
* **surround:** bind keymap the normal way ([a44fa24](https://github.com/Walheimat/wal-emacs/commit/a44fa246f8d48aa486cce97d89a16ed3eb9bc3f6))
* **vertico:** use unobtrusive for wal-project-find-in-here ([63898bf](https://github.com/Walheimat/wal-emacs/commit/63898bf6bf8914a1e8a3aff86a5657fb4074f41b))


### Bug Fixes

* **css-mode,flycheck:** select existing checker ([eeea2cd](https://github.com/Walheimat/wal-emacs/commit/eeea2cd73561a11ffe94340ec88c91a67015a680))
* **dap-mode:** don't quote :repeat config ([f1b3980](https://github.com/Walheimat/wal-emacs/commit/f1b39807927a73162ee7192d9f4a7ae858f4533f))
* **dashboard:** ignore args in advice ([9b54e5a](https://github.com/Walheimat/wal-emacs/commit/9b54e5a018639f3e9d28df7de59ff583064d97b4))
* **puni:** don't override xref-apropos ([ef6f890](https://github.com/Walheimat/wal-emacs/commit/ef6f8900e9901db4037dbd040ea3fae629bbb87c))
* **vterm:** pass arg within project ([4333663](https://github.com/Walheimat/wal-emacs/commit/43336636cee537352e489d758d483f1270e1f1af))


### Improvements

* **corfu-quick:** complete on single match ([bb1e457](https://github.com/Walheimat/wal-emacs/commit/bb1e457e6189ccebcd90cc5b369f5d03ba968ccb))
* **dap-mode:** position expressions and locals buffer at top ([6b30339](https://github.com/Walheimat/wal-emacs/commit/6b303391b348bbf6d756cbd0d7ced50644f2d46c))
* **flycheck:** add repeat map ([6e0dae1](https://github.com/Walheimat/wal-emacs/commit/6e0dae136744d68df2f4afe3c98b758746d6f15f))
* **lsp-dwim:** execute action as fallback, format for whitespace ([9868df6](https://github.com/Walheimat/wal-emacs/commit/9868df6e88bbe80a0ef5fab66aa97a5c917f85f4))

## [2.3.0](https://github.com/Walheimat/wal-emacs/compare/v2.2.12...v2.3.0) (2024-03-17)


### Features

* **lsp:** add wal-lsp-dwim and bind to user-prefixed backslash ([4a7587a](https://github.com/Walheimat/wal-emacs/commit/4a7587a46fbeead67591ef45aece78c951f1fad4))
* **rg,hl-todo:** add wal-rg-project-todos ([6cd3149](https://github.com/Walheimat/wal-emacs/commit/6cd3149de26906ea5249772adbf5e344c8a0aee7))
* **workspace:** wal-project-find-file-other-window ([9d1400c](https://github.com/Walheimat/wal-emacs/commit/9d1400cb34dbd92524786b25594c56c9d8d49869))


### Configurations

* **ace-window:** remove again ([d252185](https://github.com/Walheimat/wal-emacs/commit/d252185249ca22fb58ca704985d85e9e225e603d))
* **avy:** user-prefixed M-l jumps to word-o ([017db14](https://github.com/Walheimat/wal-emacs/commit/017db14e8a2ffe1379771d7d594781ed01191c7c))
* **consult:** bind line to user-prefixed M-i instead ([c4c7556](https://github.com/Walheimat/wal-emacs/commit/c4c7556a197c9e8885ff80b8246def29342bc8a7))
* **consult:** prefer buffer switch, transient after meta ([c36b2bf](https://github.com/Walheimat/wal-emacs/commit/c36b2bf9b60b0a668eb30bf765eefc640561804a))
* **consult:** re-bind outline, simplify theme ([fc6bb7d](https://github.com/Walheimat/wal-emacs/commit/fc6bb7d50b3d060ffc9d2dccf241aec7a68c9ff5))
* **consult:** remove wal-consult-org-agenda-buffer ([cfd58b3](https://github.com/Walheimat/wal-emacs/commit/cfd58b32a0c381f0e4bfd7f71afc21d6d2197293))
* **consult:** user-prefixed = to wal-consult-project ([a77a274](https://github.com/Walheimat/wal-emacs/commit/a77a2742a00d6842e9a6926a2b12a504ebe01a4b))
* **consult:** wal-consult-unregister in triple-minus map ([d1038dc](https://github.com/Walheimat/wal-emacs/commit/d1038dc3569aaad400388575cccbddc7655ae4e5))
* **custom:** simplify custom bindings, update list ([1906f12](https://github.com/Walheimat/wal-emacs/commit/1906f1230087cfa3ca0227a9f51aa60045d0b3aa))
* **docker,diff-hl:** bind to d and h in ambassador ([b69f903](https://github.com/Walheimat/wal-emacs/commit/b69f9035d5c48df984268eab49466a083c71157f))
* **jinx:** bind jinx-next and jinx-correct in editor ([20c5397](https://github.com/Walheimat/wal-emacs/commit/20c53975cc073f095ccd8c4297c9b84a8c42a16e))
* **magit:** remove magit-status binding from transient ([226dd53](https://github.com/Walheimat/wal-emacs/commit/226dd53f1e2c3107fe96ce382fbe33439ffdf20e))
* **org-habit:** push org-habit-graph-column back further ([271fb0d](https://github.com/Walheimat/wal-emacs/commit/271fb0d664a4ac0b35fe0f3263e5f43b255826ba))
* **other-window:** M-o other-window, C-M-o other-buffer ([9d3e12a](https://github.com/Walheimat/wal-emacs/commit/9d3e12ab370ae36da2d7493b04d4c829b1e8e22b))
* **outline:** enable for common modes, name for which key ([784a1b3](https://github.com/Walheimat/wal-emacs/commit/784a1b345e8964ec1de196cd640e8d71a7c51e8f))
* **surround:** bind surround-insert and surround-kill in editor ([622bea5](https://github.com/Walheimat/wal-emacs/commit/622bea5989b3f20e86bdf85e794a9af2940b73ff))
* **tab-bar:** bind switch to user-prefixed o, rename to M-o ([3d9201b](https://github.com/Walheimat/wal-emacs/commit/3d9201bc93702cd439912f42df348abb37fd55b7))
* **workspace:** bind custom finders in user-prefixed map ([e48a1ad](https://github.com/Walheimat/wal-emacs/commit/e48a1ad4b350dbd87090109f43cb09c6fad44db4))


### Bug Fixes

* **consult:** customize wal-consult-clock instead ([9f3c317](https://github.com/Walheimat/wal-emacs/commit/9f3c3173e33f5b0b2ece011b1925f73cbe281317))
* **jinx:** map j and c in existing repeat map ([853734a](https://github.com/Walheimat/wal-emacs/commit/853734af9bf0c4fe1fb61c1b45bfb46c2c2ecb03))
* **wal-consult-clock:** save previously and newly clocking buffer ([f396abe](https://github.com/Walheimat/wal-emacs/commit/f396abe3eea0d705e1837f47a9b4423f789716bb))
* **wal-consult-place:** don't match archived org headings ([3eb2524](https://github.com/Walheimat/wal-emacs/commit/3eb25242656aa3f4080fd81424ac10ada2c85e19))


### Improvements

* **cape:** bind dabbrev and file directly, remove prog setup ([bfebd7a](https://github.com/Walheimat/wal-emacs/commit/bfebd7a11808b5863b05c5f7a1343a7f913372ce))
* **consult,org-clock:** command wal-consult-org-clock ([e7a4dd7](https://github.com/Walheimat/wal-emacs/commit/e7a4dd75fe0ffa8d9a6aec07dca19d5af4f680f6))
* **consult,register:** user-prefixed i goes to place ([5681361](https://github.com/Walheimat/wal-emacs/commit/56813615446796f57c84ef9dbbb44bccc7a6571b))
* **flymake:** add repeat map ([eef0afd](https://github.com/Walheimat/wal-emacs/commit/eef0afd5be87e4a597b87715e44ec3365f6f0644))
* **jinx:** add repeat-map ([0aed0ab](https://github.com/Walheimat/wal-emacs/commit/0aed0ab52fdcf1c76b7130b7b79cdc3fb6152045))
* **key-bindings:** fix footnote ([dc0b602](https://github.com/Walheimat/wal-emacs/commit/dc0b6027bb3ed0a09b01e3c1845c6c8bd8a41ee7))
* **key-bindings:** remove outdated ambassador bindings ([0c2b9ad](https://github.com/Walheimat/wal-emacs/commit/0c2b9ad0d601c03cc04536df056418751a7a0dc0))
* **org-modern:** consistent filled->empty start pattern ([18400dc](https://github.com/Walheimat/wal-emacs/commit/18400dc991256b340d52a8276493e40cd1799b4c))
* **other-window:** advise to switch to buffer for single window ([3a032c0](https://github.com/Walheimat/wal-emacs/commit/3a032c0ed2f522a3fa1c12e5a4dd11b2559fed34))
* **rg:** add wal-rg-rerun-toggle-context ([8c688a5](https://github.com/Walheimat/wal-emacs/commit/8c688a5dd807f2e869123f2048957cf5f63c7ac7))
* **vertico-quick:** exit for single match ([068ba13](https://github.com/Walheimat/wal-emacs/commit/068ba13c645f9e1314932dbfdff1fce18997ccfc))
* **vertico,project:** make wal-project-find-in-here flat ([5eafd59](https://github.com/Walheimat/wal-emacs/commit/5eafd59dfc9a63ec40afe2666676c62ac71f542c))
* **windows:** remove advice again, bind other-buffer directly ([556b0dd](https://github.com/Walheimat/wal-emacs/commit/556b0dd514938af2fc64a8a8cc88349244c82761))
* **workspace:** rework structure with more subheadings ([bd11053](https://github.com/Walheimat/wal-emacs/commit/bd110533478be87f56e72563119c52bec6cee69c))

## [2.2.12](https://github.com/Walheimat/wal-emacs/compare/v2.2.11...v2.2.12) (2024-03-02)


### Configurations

* **cape, tempel:** include into CAPF ([2b2c521](https://github.com/Walheimat/wal-emacs/commit/2b2c52113d92ddbf4fef12e1cf62e5ada6feacd9))
* **consult:** bind consult-line to user-prefixed M-l ([79d6b9d](https://github.com/Walheimat/wal-emacs/commit/79d6b9d9cd98cc71877141decd8c0ab68190a2af))
* **embark:** embark-dwim with meta, remove parallel usage ([ce820c9](https://github.com/Walheimat/wal-emacs/commit/ce820c93fe1126992656e68dd68d3edbceb47733))
* **project:** bind project-find-dir ([282bd40](https://github.com/Walheimat/wal-emacs/commit/282bd40538ec7a87dcc17d6a5c429851186c26f0))
* **register:** user-prefixed i jumps, meta stores ([682100b](https://github.com/Walheimat/wal-emacs/commit/682100bf2251351c73b70d6f848dda7a27696bdf))
* **ship-mate:** bind new command to default ([d4452cb](https://github.com/Walheimat/wal-emacs/commit/d4452cbec4457befea658e95cff32d88d9a73d1e))
* **various:** use meta to access command maps or transients ([030b3c0](https://github.com/Walheimat/wal-emacs/commit/030b3c091f54cbcb7b0c076b252622c98ae6d17c))
* **whale-line:** enable whale-line-iconify ([a1fa228](https://github.com/Walheimat/wal-emacs/commit/a1fa2280b0937780c229126b5a5287aba59da639))


### Improvements

* **windows:** re-introduce ace-window ([c972d91](https://github.com/Walheimat/wal-emacs/commit/c972d9142245c09a6bd59a710028e40f315e9157))

## [2.2.11](https://github.com/Walheimat/wal-emacs/compare/v2.2.10...v2.2.11) (2024-02-24)


### Configurations

* **diff-hl:** replace command-map bindings ([a42270b](https://github.com/Walheimat/wal-emacs/commit/a42270b825e90186722a754c2ac6df3f5cbb7375))
* **elisp:** remove increased delay ([1a528ce](https://github.com/Walheimat/wal-emacs/commit/1a528ce309d9722d63f8a68c81ab7430a8772c9c))
* **magit:** bind magit-log in transient ([d8a7d7c](https://github.com/Walheimat/wal-emacs/commit/d8a7d7c2affc988bb38bd3ba6e0d28517915bfd6))
* **magit:** group transient like manual ([15bbb40](https://github.com/Walheimat/wal-emacs/commit/15bbb405fbf7ba86aab26b630bf59b5ac46d0910))
* **partial-recall:** activate new mode ([01646d0](https://github.com/Walheimat/wal-emacs/commit/01646d06fe74c7131ee2611ebc8a47735ed459ea))
* **partial-recall:** enable both new modes ([5a0d483](https://github.com/Walheimat/wal-emacs/commit/5a0d4830b901e4c535cb5f613b579c3d8dd83d6f))
* **partial-recall:** enable concentration ([ef551d3](https://github.com/Walheimat/wal-emacs/commit/ef551d369e777af67c0cdb8ac40677a2f4a3968f))
* **register:** add quick registers ([d8023c2](https://github.com/Walheimat/wal-emacs/commit/d8023c29b914696fcc671b1e5c1f7081e1bd7f57))
* **register:** swap {point=>jump}-to-register ([7cda2e3](https://github.com/Walheimat/wal-emacs/commit/7cda2e3fda71601f1fd531354f0271574004cd49))


### Bug Fixes

* **display-buffer:** don't use nw frames ([517ff8b](https://github.com/Walheimat/wal-emacs/commit/517ff8b025986910cfd9bda975c971b7553d9d75))


### Improvements

* **consult,org-agenda:** add wal-consult-org-agenda-buffer ([0bc8fb0](https://github.com/Walheimat/wal-emacs/commit/0bc8fb00d64e711a6d32225a518765d89c940e1a))

## [2.2.10](https://github.com/Walheimat/wal-emacs/compare/v2.2.9...v2.2.10) (2024-02-09)


### Configurations

* **org-capture:** unnarrowed for c, finalize t with tags ([24cd8c0](https://github.com/Walheimat/wal-emacs/commit/24cd8c0d74bdea8bf5a02017a32e7d71cb9592b6))
* **org:** ask for note when leaving blocked state ([e8169c3](https://github.com/Walheimat/wal-emacs/commit/e8169c3f6568b8aade8d5cdc762e43061ed7db25))
* **prelude:** don't hide upgrade compilation ([f9d4272](https://github.com/Walheimat/wal-emacs/commit/f9d4272d9f679636b3ba1a912628428638018ce2))
* **ship-mate:** enable newly factored out modes ([ca78302](https://github.com/Walheimat/wal-emacs/commit/ca7830212bab0cf83b621d855ac1bce1de0e02ea))
* **ship-mate:** enable ship-mate-dinghy-global-mode ([1084a8d](https://github.com/Walheimat/wal-emacs/commit/1084a8dfd3a31dc451e9638401e183e1a4d3c4c3))
* **vertico:** easier binding for vertico-multiform-vertical ([37017e8](https://github.com/Walheimat/wal-emacs/commit/37017e848b9b5111c6b849f1ca28b97446d4e72a))


### Bug Fixes

* **js:** use JSON mode for rc files ([4a5d740](https://github.com/Walheimat/wal-emacs/commit/4a5d74054f6476009b83b473a62eafcbe0286a04))
* **org-capture:** pass project to find tasks file ([e0108f2](https://github.com/Walheimat/wal-emacs/commit/e0108f277652c343073baa2f002c39e7abccbe45))
* **workspace:** allow relative paths for parent project ([ff27028](https://github.com/Walheimat/wal-emacs/commit/ff2702861d32f795d8fc2121376d5b4540a0808e))


### Improvements

* **config:** don't fold style guide ([55251b3](https://github.com/Walheimat/wal-emacs/commit/55251b3622a601cc22a7af82f7f5037a1a5580cd))
* **org-capture:** template to add new task for other project ([c7ce0f9](https://github.com/Walheimat/wal-emacs/commit/c7ce0f936ea39c85a7a4988d26c457962f426e33))
* **prelude:** ask to restart after successful upgrade ([726944e](https://github.com/Walheimat/wal-emacs/commit/726944e4022b48b38aaabcb9e8b0861aa6be1d44))

## [2.2.9](https://github.com/Walheimat/wal-emacs/compare/v2.2.8...v2.2.9) (2024-01-28)


### Bug Fixes

* **lsp,junk:** fix docstring ([6029049](https://github.com/Walheimat/wal-emacs/commit/60290499afb790eb98a59c3544d95ead330198a7))
* **pdf-tools:** declare as junk extra ([f6b47e8](https://github.com/Walheimat/wal-emacs/commit/f6b47e8d312564281003ddfa6a67070dc9d32860))
* **surround:** provide definition name when binding key ([0a1f29a](https://github.com/Walheimat/wal-emacs/commit/0a1f29a3bb60b3ebcc9ca7f256265f58f933ae9c))


### Features

* **ci:** add semantic-release ([7327138](https://github.com/Walheimat/wal-emacs/commit/7327138915e6442aefea4770ab392fa37f5d0d3a))
* **consult,flymake,flycheck:** bind wal-consult-error ([dcaa551](https://github.com/Walheimat/wal-emacs/commit/dcaa5518fb29e118912859d29cd11f3e6cdc63b5))
* **dogears:** remove package ([cb637f9](https://github.com/Walheimat/wal-emacs/commit/cb637f969c8f677982f4f14580fbd8a6dfe20789))

## [2.2.8]

Minor groove.

### Added

- `groovy-mode` and `jenkinsfile-mode` as Java `junk` extras.

### Changed

- Clocking out now prompts the user to select the next state.
- Instead of providing `wal-project-vterm`, `vterm` is now advised to
  prefer using a project-specific buffer.
- `:wal-bind[-keymap]` only supports a minimal version of
  `:bind[-keymap]`, namely keys, remaps and binding to maps.
- `typescript-mode` is now mostly configured like `js-mode`.
- Command `wal-org-agenda-take-note` now defaults to taking note for
  current task (if it exists) unless it's called with prefix argument.

### Fixed

- `:wal-bind` now handles passing a remap vector.

## [2.2.7]

Note again.

### Added

- Configured `outline-minor-mode`. All `prog-mode` modes now use it.
- Instead of only saving buffers when notes were taken for clocking
  out, taking notes always save that buffer.

### Changed

- `wal-org-clock-take-note` was removed in favor of extending
  `wal-org-agenda-take-note`. When called with argument, this will now
  take note for the clocked task.
- Key bindings were changed:
  - `H-p` still stores point, but `H-M-p` stores window register
  - `H-h` now invokes `project-find-file`
  - `H-y` invokes `jump-to-register`
  - `H-i` now completes
  - `H-M-j` now jumps to char with timer using `avy`

### Removed

- `org-clock-auto-clockout-insinuate` is no longer called. Instead
  `org-clock-idle-time` was re-added.
- `org-clock-auto-clock-resolution` is the default value again.
- `outline-minor-mode-highlight` is no longer set.
- `wal-org-content` since using `org-content` with numeric prefix is
  preferable.

### Fixed

- Clocking out now uses `wal-org-clock-out-switch-to-state` to only
  set todo state if there was one before.

## [2.2.6]

In grace you drank.

### Changed

- `consult` transient only binds two register commands now
  (`consult-register` and `wal-consult-unregister`).
- `compilation-max-output-line-length` is set to `nil`.
- `org-todo-keywords` now require notes for changes to `WAITING`,
  `BLOCKED` and `CANCELED`.
- Modes derived from `text-mode` enable `auto-fill-mode` again.
- Instead of being prompted to resolve an idle clock, an automatic
  clock out happens.

### Fixed

- `org-clock-persistence-insinuate` is now called to make
  `org-clock-persist` actually try to reload a running clock.
