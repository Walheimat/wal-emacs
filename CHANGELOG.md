# Changelog

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

## [2.2.5]

Clock and load.

### Added

- Clocking in and out now saves the buffer.
- Which non-required packages are loaded can now be customized using
  `wal-additional-packages` which includes all optional packages.
- The number of tangled files is now displayed as well as the touched
  files.

### Changed

- Calling `consult-theme` with a prefix argument now customizes
  `wal-theme`.
- Calling `wal-set-{fixed,variable}-font-height` now sets the
  respective custom variable.
- Clocking out now puts task state to `WAITING` by default.
- Swapped `C-c T` and `C-c t` (clocking out and taking note).

### Fixed

- The size of the animation is now updated when fonts are updated.
- `wal-consult-clock-in` now only matches non-archived and active
  headings.
- Open projects are no longer hidden.
- `ship-mate` compilations use `wdb-faraway` again.

## [2.2.4]

Binding light.

### Added

- `wal-org-agenda-take-note` to do just that, bound to `C-c n`.
- Package `gumshoe` as alternative to `dogears` (might remove both
  again soon).
- `dinghy-rope` is now installed during `ensure`
- `winner` now ignores buffers that use a buffer because of `vertico`.
- The newly free `H-p` and `H-y` are temporarily bound to
  `point-to-register` and `jump-to-register`.

### Changed

- `vertico-sort-function` is no longer customized (meaning it uses
  `vertico-sort-history-alpha` again).
- `repeat-exit-timeout` was increased to 5 seconds.
- Bindings for `multiple-cursor` repeat map have been made more
  ergonomic.
- `org-clock` bindings now use `C-c {t,T}`.
- All `consult` commands are now bound in `consult` `transient`.
- The concept of pre-narrowing `consult` commands was removed again.
- `consult-buffer` and `project-find-file` now use `vertico-multiform`
  style `buffer`.
- `H-j` now does what `H-u` used to do: go to word, quick exit with
  selection.
- `wal-project-dired-root` was removed (since `project-dired` does the
  same thing).
- Custom `eshell` command was removed; `vterm` one now uses
  `project-prefixed-buffer-name` like `project-eshell`.

### Fixed

- `org-habit` no longer force-loads `org-agenda`.

## [v2.2.3]

Whale form.

### Added

- `wal-update` now calls script that uses `package-vc-upgrade-all` to
  upgrade all `wal-bridge` and eventually tangles.
- `C-c v` is bound to `wal-org-capture-project-tasks`.
- While editing this config, the swimming whale `posframe` is
  displayed again for project buffers. Can be disabled by setting
  `wal-config-show-animation` to `nil`. All variables and functions
  have been renamed from `wal-ascii-whale-*` to
  `wal-config-animation-`.
- Package `surround`.
- Package `dogears` now has custom command `wal-dogears-list` which
  pops to or toggles the list. It was moved to `find` package.

### Changed

- `rg-isearch-current-file` is now bound to `M-s n` instead of `H-n`
  when `isearch` is active.
- `dogears` has idle timer enabled again and hooks removed.
- Commands executed through the prelude's `wal--compile` function
  don't display the buffer anymore. They can be displayed using new
  command `wal-show-compilation-result` bound in `whaler` to `r`.

### Fixed

- `org-agenda` is no longer implicitly loaded by `partial-recall`.
- `markdown-mode` and `org-mode` no longer enable `flycheck-mode`.
- `magit-process-buffer` has double the size.

## [v2.2.2]

Extrinsic vice.

### Added

- `other-frame` is now bound in `other-window-repeat-map`.
- Package `dogears` as a trial.
- My package `ship-mate` which is functionality spun out from this
  config.

### Changed

- Fringe mark for `bookmark` was disabled.
- `org-capture-templates` was reworked. Plain `p` now is for plain
  entries in the file (unnarrowed) while `t` is adding new tasks.
- [The following are included in `ship-mate`]
  - Commands added to (formerly) `wal-project-command` history no
    longer replace the one matched against. They're just inserted.
    [This is a feature of `ship-mate`].
  - `recompile` is now advised to set the `compile-history` to that of
    the last (formerly) `wal-project-*` command if the
    `compile-command` matches.

### Fixed

- [The following are included in `ship-mate`]
  - Updating the history of (formerly) `wal-project-*` commands now
    uses fuzzy matching instead of just checking the last
    `compile-command` (as this can lead to erroneous replacements).
  - (formerly) `wal-project-command` no longer infinitely extends a
    command's history.

## [v2.2.1]

Itchy and patchy.

### Added

- `wal-update` to invoke all necessary commands and Makefile targets
  to have the latest changes installed.
- `wal-project-command` now tries to update its history when
  `compilation-start` is run.

### Changed

- `compile` is bound again to `C-c i`.

### Fixed

- `wal-scratch-persist` no longer persists the
  `initial-scratch-message`.
- Additional scratch buffers are prepped correctly (initial message
  and mode).

## [v2.2.0]

Prelude to peace.

### Added

- Local variable `wal-org-capture-tasks-file` can now also be set to a
  path relative to `org-directory`.
- Package `org-habit-stats` as an extra of new `junk` expansion `org`.
- `org-stuck-projects` is now configured to use tag group "energy".

### Changed

- `org-agenda` buffers are ignored by `partial-recall`.
- Prelude package was renamed from `wal-prelude` to just `wal` after
  linting (requires re-running `make local` to update the
  bootstrapper).
- All library files are now tangled using `wal--tangle-target`.
- All internal path variable were renamed.
- Persistent tag `@growth` was replaced with `@wellbeing`.
- Functions `wal-package-{installed,built-in}-p` were moved to
  `wal-package`.
- `wal-consult-project` is now called with `:require-match t`.

### Fixed

- `all-the-icons-dired-mode` is now advised to not be triggered for
  non-GUI frames.
- `wal-ignore-if-not-installed` now uses `wal-package-installed-p` to
  make sure missed built-ins aren't ignored if they set `:hook`.
- Custom prefix map `wal-project-prefix-map` is now bound directly to
  avoid losing the prefix argument on first invocation.

## [v2.1.14]

Minimal comfort.

### Added

- Command `wal-org-clock-kill-current-task` to do just that.
- The minimal setup (with flag `--mini` or customizing `wal-minimal`)
  is now actually minimal, loading (for now) only built-in packages
  and marked packages.
- Initial scratch message now contains a comment about the setup.
- Custom variable `wal-hyper-mock` that defaults to `C-c w`. It is
  bound in `function-key-map` to apply hyper modifier. This makes
  `wal-use-hyper-prefix` obsolete.
- Package `corfu-terminal`.

### Changed

- Custom `use-package` keyword `:wal-ways` now defaults to `nil`.

### Removed

- Custom variable `wal-minimal-exclude`.
- Custom variable `wal-use-hyper-prefix` (see above).

### Fixed

- Macros for `wal-capture-flag` were moved to `wal-package` since
  otherwise packages in `wal-bridge` are not affected.

## [v2.1.13]

Fishing for complements.

### Changed

- `dashboard-refresh-buffer` is now bound in `ambassador` to `0`.
- Binding for `compile` (`C-c t`) was removed.
- Org tags are now grouped and no longer define select characters.
  This is because of the fix below; files can now freely define their
  own tags and both the predefined and the file-specific tags can be
  chosen.
- The package tags are now grouped under `package`.
- Neither `flycheck-mode` nor `flymake-mode` hook into `prog-mode`
  anymore. This is because `harpoon` now allows to set a checker
  function that can be overridden (which is done for `emacs-lisp-mode`
  and `gdscript-mode`).

### Fixed

- `org-tag-persistent-alist` is now set instead of `org-tag-alist`.

## [v2.1.12]

Visible introspection.

### Added

- Better package documentation.
- Local variable `wal-org-capture-tasks-file`. When set, locating the
  heading will try that file first. This allows tasks file living
  outside of a project again.
- Command `wal-config-org-tags-view` to search for tags in the config.
  When called with `C-u` matches all package tags.
- Adds tags to all package configurations to indicate where they are
  sourced from.
- Command map for `diff-hl` is now bound to `d` in `ambassador`
  (`docker` was moved to `o`).

### Changed

- Function `wal-insert` now uses keyword arguments for
  `allow-duplicates`, `before` and `quiet`.
- Binding for `j` in `dired-jump-map` is removed because it interferes
  with `dired-goto-file`.
- Bindings in `wal-project-prefix-map`.
- Completion of files with `cape` now uses `C-c /`.
- `emacs-lisp-mode` now uses `flymake`.

### Fixed

- Binding to `M-o` in `sgml-mode`'s `html-mode` is rebound to `C-M-o`
  in order to not interfere with `wal-switch-to-other-buffer`.

## [v2.1.11]

Longer safe.

### Changed

- Setting `project-vc-name` is marked as safe for strings.
- Setting the various custom local variables is now considered safe.
- `consult` is no longer advised to put the current buffer first.
- `gdscript-mode` was updated to use `eglot`, `flycheck`, and
  `wal-maybe-enable-tabs`.
- Package configurations are no longer divided into **Utility** and
  **Configuration** sections.

## Removed

- Commands `wal-tab-bar-switch-to-buffer-tab` and
  `wal-tab-bar-rename-from-project`.

## [v2.1.10]

Prompt inaction.

## Added

- `profiler` commands are now bound in `administrator`.
- The bootstrapper is no longer required to live at the end of the
  init file (although it will always be appended to the end on
  creation).
- `cargo-process-run-example` is now bound in Rust `major`.
- Tangling the config now notifies immediately.

### Changed

- `proselint` checker is disabled (has to be enabled manually).
- `wal-project-command` switches to `comint` with numeric prefix
  argument 0.
- `wal-project-command` no longer prompts unless called with a prefix
  argument as long as the history is non-empty.
- `consult-buffer` and `tab-switch` use `flat` completion.
- Autosave variables have been adjusted downwards (more autosaves).
- `multiple-cursors-mode` is now "prominent" in `minions`.

### Removed

- `wal-project-create-command` no longer accepts key `:comint`. All
  commands use `compile` by default again; see above.
- Functions `wal-delete-edit-or-kill`, `wal-pad-string` and
  `wal-reset-to-standard`.

### Fixed

- `wal-read-sensible-font-height` now uses `face-attribute` to
  retrieve the current value; no command sets custom variables
  anymore.
- Open and closed projects are now differentiated during completion
  (no duplicates).

## [v2.1.9]

Valued locals.

### Added

- Command `wal-switch-to-other-buffer` bound to `M-o`.
- Commands created by `wal-project-create-command` now also create
  variable `wal-project-{cmd}-reverse-mode` that allows overriding the
  default usage (`comint` or `compile`) for the given command.

### Changed

- Pre-narrowing in `consult` is off by default.
- `dired-auto-revert-buffer` is now set to
  `dired-directory-changed-p`.
- `savehist-additional-variables` is now set to `(kill-ring)`.
- `xref-search-program` is set to `ripgrep` if possible.
- `vertico-multiform-commands` now set `{switch-to,consult}-buffer`
  and `tab-switch` to use `unobtrusive`. Previous configurations are
  done via `vertico-multiform-categories`. Switching to the vertical
  view and back is bound to user-prefixed `i`.
- `initial-major-mode` is no longer set to `fundamental-mode`.
- `vertico-resize` is set to `t`.
- `transient` for `consult` now calls either `consult-org-heading` or
  `consult-outline` depending on the mode.
- User-prefixed `i` now uses `avy-goto-char-timer` with a timer of 0.4
  seconds instead of `avy-goto-char`.
- Project task `org-capture` template no longer adds the file path.

### Fixed

- `partial-recall` buffer source is no longer inserted before the
  default buffer source.
- `corfu-popuinfo` is no longer ensured.
- `make test` running `cask install` since the test folder might be
  more recent than the build folder (see below).
- `wal-project-local-value` now uses `project--value-in-dir` instead
  of visiting the root buffer and using `buffer-local-value`.
- The custom `dashboard` banners are now chosen by new override advice
  to `dashboard-choose-banner`. So having both GUI and terminal frames
  should yield the appropriate logo.
- `org-capture-templates` now set empty lines before and after instead
  of using newline.

### Removed

- Command `wal-consult-line` as `consult-line` already adds
  `thing-at-point` to history.
- Command `wal-consult-org-heading` as narrowing covers most cases.
- Package definitions in `Cask` file as installing the package via
  `cask` is not (yet) possible.

## [v2.1.8]

Dapper capper.

### Added

- Package `cape`; `cape-history` is bound for `eshell`.
- Configuration for `corfu-popupinfo` to replace `corfu-doc`.
- The `company` backend `dap-mode` is now adapted to work with
  `corfu`. This is done by overriding `dap-ui-repl-company-prefix` and
  mapping the backend with `cape`'s adapter function.
- `woman` is now bound in `administator.

### Changed

- `dired-hide-details-mode` is now turned on by default.

#### Removed

- `corfu-doc`.

## [v2.1.7]

Near prudence.

### Added

- `wal-project-command`s can now have multiple defaults that will be
  added to the commands history on creation. For example, settings
  `wal-project-build-default-cmd` to `'("make tangle" "make local")`
  will add both commands to the history leaving `"make tangle"` as the
  most recent entry.
- `repeat-exit-key` is now bound to `<return>`.
- `wal-org-hide-emphasis-markers` that will enable them (or disable
  them if called with a prefix argument) and fontify the buffer.
- `wal-insert` that works like `wal-insert-before` but has optional 5th
  argument to insert before instead (pushing previous 5th argument
  `quiet` to position 6).
- `use-package` keywords `:sinker` to add hook functions at the end
  and `:fhook` to add a hook to a `-functions` variable.

### Changed

- `org-hide-emphasis-marker` is no longer set as `t` (see above).
- `multiple-cursors` now uses a repeat map binding more commands.
- The only `wal-project-` commands created now are `build`, `install`,
  `clean`, `run`, new `execute` (the non-interactive equivalent of
  `run`) and `test`.

### Removed

- `wal-{make,run}[-*]` commands as the `wal-project-{*}` cover all scenarios
  now.
- `wal-insert-before` was removed (see above).

### Fixed

- `consult-flycheck` binding in `consult` `transient` no longer
  overrides `consult-recent-file`.

## [v2.1.6]

Turtle hour.

### Added

- `wal-project-update`.
- `wal-eshell` as a project-aware wrapper around `eshell`. It will
  create a new `eshell` buffer for a project or pop to an existing
  one.
- `vterm` and its utility functions were re-added and slightly
  amended.
- `wal-tab-bar-rename-from-project` to do just that.

### Changed

- The `corfu-auto-{delay,prefix}` were changed (or removed) for
  several languages.
- `eshell` buffers are now displayed at the bottom in a dedicated side
  window.
- `wal-project-{install,publish}` now use `comint-mode`.
- `wal-project-command`s now accept a prefix argument to toggle the
  mode they usually use and will give accurate information in their
  docstrings.

### Fixed

- `wal-run-test` now uses correct `bydi` function and loads
  `bydi-report`.
- `comint-mode` buffers are now part of the `consult` source for
  compilations.
- `wal-eshell` now requires `eshell`.

## [v2.1.5]

Here but mostly there.

### Added

- Key `:comint` that can be passed to `wal-project-create-command` to
  make sure a command is run in `comint-mode` instead.
- Command `wal-project-run` that uses the new option.
- Compilation buffers can now be narrowed using `c`. This `consult`
  source uses new function
  `wal-switch-to-buffer-obeying-display-actions`.

### Changed

- Subsections in `wal-emacs` were removed.
- `search-whitespace-regexp` was set to allow for matching any char
  with space.
- `consult-imenu` and `consult-outline` are now bound in `consult`
  transient.
- `dired-kill-when-opening-new-dired-buffer` is no longer enabled.
- When locating project tasks, the user is prompted to select a
  project if not in a project currently.

### Removed

- `wal-major-delight` and the silly names for certain major modes that
  were ignored in `treesit` variants.
- Custom bindings for `jinx` were removed.

### Fixed

- The advice added to `recompile` for `wal-project` commands no longer
  breaks its functionality outside of projects (notably affecting
  `rg`).
- Functions `wdb-nearby` and `wdb-farway` no longer add nil
  parameters. This might also have led to issues where windows could
  no longer be targeted by `other-window.`

## [v2.1.4]

Binky-brained.

### Added

- Command `wal-project-find-in-here` to find a project file in the
  `default-directory`.
- New project command `coverage` defaulting to `make coverage`.

### Changed

- Function `wal-insert-after` now allows passing optional fourth
  argument to quietly ignore a no-op instead of raising a user error.
- `org-mode` no longer runs `auto-fill-mode`. Its `major` now binds
  `visual-line-mode`. Package`visual-fill-column` is now explicitly
  declared as a package and runs when `visual-line-mode` is run.
- Command `wal-supernova` now quits windows until no target remains.
- Macro `wal-project-create-command` now uses `cl-defmacro` and has
  optional keys `key` (to not default it) and `default` to set an
  initial command.
- `org-capture` and custom `wal-org-capture-switch-to-project-task`
  are no longer part of the `transient` for `org-roam`. They're not
  bound using a `parallel` to `C-c c`.
- `smerge` now uses `C-c g` and `recompile` uses `C-c r`.

### Removed

- Helper package `wal-pacify` was moved to `dinghy`.

## [v2.1.3]

Prism break.

### Added

- Default value variables created by macro
  `wal-project-create-command` are now marked as `safe-local-variable`
  so that they don't need to be added manually every time.

### Changed

- `wal-use-package-ensure-elpa` no longer checks if a package is a
  `junk` package since this is now a feature of `junk` itself when
  `junk-use-package-setup` is called.
- Switched to using `dinghy`.
- `wal-settings` now considers `find-sibling-rules` a safe variable.

### Removed

- `consult-buffer-filter` is no longer customized.

### Fixed

- `wal-project-*` commands are only added to history if not present.

## [v2.1.2]

Far-away troubles.

### Added

- `org-agenda-todo-yesterday` is now bound in `org-agenda-mode-map`.

### Changed

- All test suites now new the newly added `bydi` patterns.
- Package `wal-line` was renamed to `whale-line`.
- `commitlint` rules now include fixed types.
- `wal-core-vc-packages` now use specs.

### Removed

- Function `wal-matches-in-string` as well as
  `wal-check-coverage--{calculate-coverage,add}` which are now part of
  `bydi` package.
- `junk--pack-p` is only used if bound.
- Package `whale-line` is loaded after `all-the-icons`.

### Fixed

- Docstring `tempel` templates should work (a bit) better.

## [v2.1.1]

Bridge twofer.

### Added

- New library file `wal-bridge` contains all of my own packages.

### Changed

- `partial-recall` was spun out as its own package.
- `harpoon` was spun out as its own package.
- `junk` was spun out as its own package.
- Testing macros were spun out as their own package as `bydi`.
- Pre-narrowing is now always enabled for `consult`.
- `wdb-{faraway,nearby}` now default to
  `display-buffer-use-some-window`.

## [v2.1.0]

Pizza time.

### Added

- The internal structure of `wal-partial-recall` (see below) now uses
  `cl-defstruct` to create memories and moments; memories are rings of
  moments and an original size; moments are buffers and timestamps
  that refer to when they were added; the ring will now grow if adding
  a new buffer would kick an existing buffer but that buffer is not
  older than `wal-partial-recall-threshold` (new custom variable,
  defaults to 60) seconds; buffers that belong to a different memory
  are reclaimed if visited when `wal-partial-recall-reclaim-threshold`
  (5 minutes by default) is exceeded.
- Custom variable `wal-consult-buffer-narrow-source` with options
  `recall` (the new default) and `project` that determines which source
  to pre-narrow to.
- Custom variable `wal-consult-pre-narrowed` commands that is set to
  `consult-buffer` to give more control of which commands should be
  narrowed if toggled on.
- The format used in `wal-run-test` and `wal-run-test-file` can now be
  toggled between `text` and `json` using
  `wal-run-test-toggle-format`.
- Tool package `wal-pacify` that checks all tangled files for
  byte-compile and doc issues using `flymake` now has a test suite.
- Convenience command `wal-config-load-test-helper` to do just that.
  Bound to `e` in `whaler`.

### Changed

- `wal-tab-buffers-*` functionality was renamed to
  `wal-partial-recall-*`. It is still associated with tabs. See above
  for new functionality. Tabs now get a custom key that is created on
  tab creation to keep track of their buffer history. This means tabs
  no longer need an explicit name to have one and that renaming them
  doesn't wipe out their association with their history.
- Recall limit can now be customized through `wal-total-recall-limit`.
- `whaler` was restructured.
- The ASCII whale animation is now done using indirect buffers meaning
  it can be enabled and disabled for individual buffers.
- `wal-run-test-file` now reads from test directory.
- The config buffers are no longer ignored by `consult`.

### Fixed

- Package `wal-settings` no longer attempts to create a site-lisp
  directory to make sure it only actually sets things (it will still
  recursively load it).
- Package `wal-settings` no longer defines any functions (moved to
  `wal-useful`).
- Package `wal-settings` no longer creates key bindings; this was
  moved to `wal-key-bindings`.
- All packages now explicitly require the packages they depend on.
  Only relevant if `wal-packages` is altered before bootstrapping.
- `wal-agenda-buffer-p` now uses `org-agenda-file-p`.
- Some unnecessary code was replaced in tests to make them run faster.
- Table entries for `wal-partial-recall` are deleted on
  `delete-frame`.

## [v2.0.3]

Name face.

### Added

- Tab buffers. Tabs that have been explicitly named now have a history
  of buffers found in their context. This is used in a `consult`
  buffer source that can be narrowed to using `t`.

### Changed

- Packages `font` and `look` were folded into `visuals`.
- `wdb-nearby` and `wdb-faraway` were refactored to accept (mostly)
  the same key arguments. `wdb-faraway` now has key parameter `bottom`
  to use `display-buffer-at-bottom`. `wdb-nearby` now allows setting
  either `direction` or `side`. Window widths and heights are no
  longer defaulted.
- `python-mode-major` no longer binds individual `send` commands.
  `inferior-python-mode` now enables `corfu`.
- Package `wal-func` was renamed to `wal-useful`.
- Package `wal-external` was renamed to `wal-package`.
- Running `make` alone should install everything now, including
  packages.
- Default setup for `org-src` is used.
- Customization is no longer part of `wal-settings`. It's now part of
  `wal-prelude` which makes the settings package optional.
- `wal-prelude-bootstrap` now only takes a single optional argument
  that sets its mode (`plain`, `cold`, `ensure` or default `normal`).

### Removed

- Commands `wal-set-indent-defaults` and `wal-reset-indent-defaults`.
  Indentation variables are customized in their respective packages
  instead.

### Fixed

- `wal-prelude` now renders the contents of the `init.el` template
  using the source directory. This should allow for the clone not
  having to live in the `user-emacs-directory` and have any chosen
  name.
- The messages buffer no longer has conflicting `display-buffer`
  entries.
- `lsp-ui-doc-show-with-mouse` is now disabled to avoid messing with
  help echos.
- Running Emacs with `--ensure` should no longer attempt to install
  built-in packages.
- Running `wal-prelude-tangle-config` now touches files in
  `wal-prelude--phony-build-dependencies` to avoid needless `cask
  install` runs.
- `switch-to-buffer-obey-display-actions` is no longer set to `t`
  since it leads to buffer position being lost on `switch-to-buffer`.

## [v2.0.2]

Check make.

### Added

- Command `recompile` is now bound to `C-c c`.
- Utility function `wal-project-local-value` that uses
  `buffer-local-value` on the project's root folder.
- Script `tools/update-version.sh` to update references to old tags.
- Command `wal-supernova` to quit windows displaying otherwise hidden
  buffers. Bound to `C-c o`.

### Changed

- Tool `wal-pacify` now uses `flymake` instead of `flycheck` to prep
  it for running in CI.
- Commands `wal-check-coverage` and `wal-create-json-coverage` were
  replaced by `wal-run-test` that runs `make test` and will
  create JSON coverage when called with `C-u`.
- Command `wal-run-test-file` to select a single file to run.

### Fixed

- Order in `wal-prelude` to allow `cold-boot` to work again.

## [v2.0.1]

Pleasure island.

### Added

- Command `wal-org-capture-switch-to-project-tasks` is now bound in
  `project-switch-commands` to `t`.
- Local variable `wal-project-parent-project` used in
  `wal-org-capture--find-project-tasks-heading`.
- Command `wal-project-switch-to-parent-project` using local
  `wal-project-parent-project`.
- `wal-prelude` now has a test suite and full coverage.
- `js-mode` and `java-mode` now enable `subword-mode`.

### Changed

- Function `wal-org-capture--find-project-tasks-heading` now gets the
  buffer-local heading as well buffer-local
  `wal-project-parent-project`.
- Package `diff-hl` was moved to `wal-vc`.
- Data files `init.eld` and `tempel.eld` now live in `data/`
  directory.
- Command `wal-config-root` was replaced with
  `wal-config-switch-project` to make use of
  `project-switch-commands`.
- User-prefixed `k` quick-completes during `vertico` and `corfu`
  completion.

### Fixed

- Package `diff-hl` is now loaded after `magit` again to avoid the
  hooks being possibly overridden.

### Removed

- Templates in `templates/` directory (data files were moved, see
  above).

## [v2.0.0]

Crank action.

### Added

- Command `wal-config-consult-org-heading` to find a heading in any of
  the library files (see below).

### Breaking Changes

- Installation now uses a `Makefile`; running `make install` should
  prepare everything (other than installing external packages).
- All code is now under `lib/` in separate Org files. The library
  files are prefixed with `wal-` so their buffers can be ignored in
  `consult`.
- The base package is now called `wal-config`, not just `wal`.

### Changed

- The `pacify` utilities were moved to `tools/wal-pacify.el`.

### Removed

- All files under `setup/`; all scripts other than the daemon setup
  have entries in the `Makefile`.
- Documents under `documents/`; the style-guide was integrated in
  `lib/wal-config.el`, the bindings and hyper guide in
  `lib/wal-key-bindings.el`, the debug templates in `lib/wal-lsp.org`.
- `wal/find-config` (which no longer makes sense, see above).

## [v1.12.1]

Tab start.

### Added

- Custom `consult-org-heading` that when called with
  `universal-argument` limits the scope of the completion to the
  current tree and doesn't use unobtrusive `vertico` completion.
- `tab-bar-mode` is now enabled but the tabs themselves are invisible.
  Switching tabs has user-prefixed `o` as a convenience binding. New
  tabs start with a `dashboard` buffer.
- `dired-do-query-replace-regexp` is now bound in `dired-mode-map` to
  `% r` to replace duplicate `dired-do-rename-regexp` binding.
- `treesit` and `lsp` are now enabled for `c-mode`.
- Function `wal/tab-bar-switch-to-buffer-tab` to switch to an owning
  tab. Bound in `embark-buffer-map` to `t`.
- Command `wal/org-capture-switch-to-project-tasks` bound in `roamer`
  using `p`.
- `c-mode` will now run `lsp` and `treesit` if possible.

### Changed

- `eww` and `eshell` are now bound to `C-c b` and `C-c e` respectively
  instead of in `administrator`.
- User-prefixed `i` is now bound to `avy-goto-char`.
- The `major` `transient` for `org-mode` was improved.
- `wal/org-agenda-file` is now a custom variable.
- `switch-to-buffer-obey-display-actions` is now set to `t`;
  `switch-to-buffer-in-dedicated-window` to `prompt`.

### Fixed

- Packages installed using `package-vc-install` are now added to
  `package-selected-packages` using
  `package--update-selected-packages` to make sure they are persisted.
- Hooks are no longer ignored for user-selected packages (mainly
  affects `vertico-directory`).
- `diff-hl-magit-pre-refresh` is called on `magit-pre-refresh-hook`.

## [v1.12.0]

Fountain boots.

### Added

- Custom `use-package-ensure-function` that also checks that packages
  aren't built in nor part of an expansion pack.
- Function `wal/browse-html-file` is now bound to `x` in
  `embark-file-map`.
- Variable `wal/core-vc-packages` of packages to be installed using
  `package-vc-install` if present; only package there is `wal-line`.

### Changed

- Package `winner` was moved to `wal-windows`.
- Non (M)ELPA packages are now installed using `package-vc-install`.
  This only affects package `wal-line` which will no longer be loaded
  in Emacs < 29. `junk` recipes also use that method with new function
  `junk-package-vc-install`.
- Templates are now completed using user-prefixed `\`.
- All coverage files are now created in `coverage/`.
- `custom-file` is now created using `make-empty-file`.
- `helpful-mode` uses `pop-to-buffer` again.
- User-prefixed `o` is bound to `consult-register-store`.
- Command `kubernetes-overview` is now bound in `ambassador`.

### Fixed

- Only wrapping `wal/project-command` is now advised with
  `wal/with-project-bounded-compilation` to avoid being asked to save
  unrelated buffers when calling a generated command while switching.
- `prettier` errors are advised to be shown as warnings so that they
  can be muted once and for all.
- Recipes are now correctly determined as packages in `junk--pack-p`.
- Package `slime` is now deferred.

### Removed

- Configurations for `tabulated-list`, `hideshow`, `ediff` and `dictionary`.
- All `:ensure t` settings in `use-package` forms that are no longer
  needed.
- Package `popper`.
- Package `diminish`.
- Packages `quelpa` and `quelpa-use-package`
- Package `iedit`.

## [v1.11.5]

Plunk into clunk.

### Added

- `rg-isearch-current-file` is now bound in `isearch-mode-map` using
  user-prefixed `n`.
- Capturing (project) tasks now uses local variable
  `wal/org-capture-tasks-heading` to find the file and heading using
  new function `wal/org-capture-find-project-task-location`.
- Command `wal/force-delete-other-windows`; bound in
  `other-window-repeat-map` using `C-k`.
- Package `jinx`.
- Command `wal/spill-paragraph` to do the opposite of
  `fill-paragraph`.

### Changed

- All leaders no longer have the `wal/` prefix.
- As the directories were removed, `org-agenda-files` is now single
  file `.agenda` in the `org-directory`.

### Fixed

- Custom `flycheck` checkers are now registered.

### Removed

- **Personal** section in favor of using a `default.el` file in the
  `site-lisp` directory.
- All custom directories other than the `wal/site-lisp-directory`.

## [v1.11.4]

Group effort.

### Added

- New function `wal/plist-keys` to extract keys from a plist.
- New command `wal/project-select-command` that replaces
  `wal/project-compile` in `project-switch-commands` to select any
  registered command.
- New command `wal/rg-project-literal` to do a literal search in
  project. This command also replaces the `project-switch-commands`
  search.
- `C-c n` is now a `parallel` command of `wal/org-clock-take-note` and
  `org-clock-goto`.

### Changed

- Pre-narrowing `consult` can now be toggled; pre-narrowing is off by
  default.
- `dap-mode` is now bound in `ambassador` (was bound in `lsp-mode`
  `transient` before).
- Custom command `wal/org-clock-take-note` was moved to `C-c n`
  (previously bound in `editor`).
  `wal/kill-ring-save-whole-buffer` was moved to `editor` (previously
  bound in `triple-minus`).
- Other bindings to delete, goto and display from `windmove` are now
  bound.
- User-prefixed `i` uses `parallel` again to either load or store
  register.
- `org-mode` `transient` was simplified slightly.
- `transient`s `whaler`, `consult` and `org-mode-captain` now have
  top/bottom grouping.
- Most `wdb/nearby` buffers allow getting targeted by `other-window`
  again.
- Renamed `proselint` configuration and disable exclamation check.
- `winner` and `windmove` now hook into `emacs-startup-hook`.

### Removed

- Package `vterm` has been mothballed.
- `transient` for `lsp-mode` in favor of binding the command-map at
  top level.

### Fixed

- Duplicate binding of "f" in `consult` `transient`.
- Regex for `junit` errors should now also work in `compile` when run
  by `mvn`.
- `wal/project-command` now resolves the default command value from
  the targeted project.
- The `Caskfile` now requires `compat` to not run into any
  compatibility issues in the pipeline.
- `org-tree-slide` resets text-scale adjustments after stopping.

## [v1.11.3]

Demilitarized keyboard.

### Changed

- Leader key `general` was renamed to `administrator`; leader key
  `captain` was renamed to `major`; `major` was renamed to
  `ambassador`; `colonel` was renamed to `whaler`; `trooper` was
  renamed to `roamer`; `lieutenant` was renamed to `editor`.
- `popper` is now bound to user-prefixed `\`; cycling is done adding
  meta key.
- `text-mode` now activates `flycheck` but is set up to only check on
  save.
-  Macro `wal/define-init-setup` now implies `wal/on-boot`.

### Added

- Configuration for built-in `windmove`.
- `treesit` and `lsp-mode` is enabled for `yaml`.
- Regex matching for JUnit errors.
- Advice to delay the popup of `transient`s; used by leaders `whaler`
  and `consult`.
- `flyspell` now has a repeat map and the default map is overridden by
  `wal/flyspell-map`.

### Fixed

- `electric-indent-mode` is disabled for `multiple-cursors-mode`.
- Undos `treemacs` attempt to force ignore its buffers (which is
  mainly annoying for `dap-mode` buffers).
- `wal/other-window` has a repeat map like the original
  `other-window`.
- `js-ts-mode` has the same templates as `js-mode`.

### Removed

- Package `ace-window` has been mothballed.
- Custom `wal/other-window`.

## [v1.11.2]

Echo gecko.

### Added

- `vertico-multiform-mode` is now enabled; several `consult` commands
  that concern visiting places in buffer now use the `buffer` display.
- Command `wal/rg-rerun-toggle-hidden` to do just that.
- `consult` is now a `transient`.

### Changed

- `wal/async-process` now re-uses the same buffer in
  `compilation-mode` so `recompile` can be used.
- `consult-buffer` queries that sort by visibility are advised to
  still put the current buffer first.
- `transient-show-popup` is now `t` again and
  `transient-mode-line-format` shows the buffer identification.
- `wal/kill-some-popups` now only asks for confirmation if called with
  prefix argument.
- `popper-echo-mode` is no longer on.
- `wal/consult-line` was inverted (again).
- CI jobs are now grouped by branch. The `pre-push` hook was moved to
  a CI job that is only triggered by message "pacify".
- Various bindings.
- Some variables were renamed to reflect intent over content.
- Commands to test the config now share function `wal/run-script`
  instead of their own implementation.

### Removed

- Package `goggles`, `code-review`, `forge`, `pug-mode`, `po-mode`,
  `graphql`, `jakt-mode`, `jenkinsfile-mode`, `tokei` and `typo-mode`
  were mothballed.
- Command `wal/push-mark`.
- Configuration for `zone`.

### Fixed

- All commands created with `wal/project-create-command` are now
  advised using `wal/with-project-bounded-compilation`.

## [v1.11.1]

Peak conformance.

### Added

- Macro `wal/project-create-command` that defines everything necessary
  based on a single symbol.
- Commands `wal/set-{fixed,variable}-font-height` now read the prefix
  argument to apply their changes only to the current frame.
- Command `wal/perform-cold-boot` to run the `cold-boot.sh` script
  interactively.
- Setting `which-key` replacements is now done using new macro
  `that-key`.
- `pre-push` hook that will run `flycheck` against the package files.
- Command `wal/mwim-beginnnig` to jump to `beginning-of-line-text.`
- Utility `wal/lsp-ignore-directory` that allows just passing
  directory names (used by `harpoon`).
- Custom `flycheck` checker for `less-css-mode`.

### Changed

- Value for `transient-show-popup` is `t` locally in
  `magit-status-mode`, meaning there's no delay.
- Command `wal/project-switch-project` was replaced by
  `wal/consult-project` that pre-narrows to open projects.
- `wdb/faraway` now checks for mode windows as well.
- `helpful` now uses `display-buffer` over `pop-to-buffer`.
- Keymap for `flycheck` was moved to `wal/major`.
- Binding for `completion-at-point` is now local when using `corfu` in
  `harpoon`.
- `wal/consult-clock-in` now clocks discontinuously if called with
  `C-u`.
- `consult{-global}-mark` no longer uses `parallel`.
- Max refile depth was increased by one.
- `smerge` now uses prefix `C-c r`.
- `flycheck`'s prefix map is now bound in `wal/major`.

### Removed

- Command `wal/split-window-the-other-way`.

### Fixed

- `wdb/faraway` now sets `inhibit-switch-frame` to do just that.
- Functionality concerning external packages was moved from
  `wal-settings` to `wal-external`.
- Variable name `json-encoding-default-indentation` when resetting
  indent defaults.
- `wal/kwim` now passes prefix argument to `kill{-whole}-line`.
- `org-refile` no longer passes prefix argument if it is numeric
  argument 5 (so that things can be refiled and not just visited).
- The JSON parsers for `prettier` should now also work for other JSON
  modes.

## [v1.11.0]

Silicon mountain.

### Added

- Following recipe from its wiki, `consult-buffer` is now pre-narrowed
  to project buffers by default. This is controlled by new custom
  variable `wal/consult-buffer-narrow-to-project`.
- Command `wal/project-switch-project` that filters by open projects.
  User-prefixed `h` now uses a `parallel` of this and the original.
- Utility function `wal/display-buffer-same-place-or-faraway` to
  display a buffer in another frame if possible, reusing its window,
  and only as a last resort in a pop-p window. This is used for custom
  `project` command buffers.
- Keymap `wal/zero-in` to bind finding commands.
- Keys `[` and `]` were added to `wal/key-reach`; leaders
  `wal/consult` and `wal/trooper` were added to map `wal/consult-map`
  and `wal/org-roam-captain`.
- Macro `harpoon` now has a mapping for modes whose `treesit`
  equivalent does not follow the `<name>-ts-mode` pattern.
- `js-json-mode` (and `json-ts-mode`) now have a `harpoon`.

### Changed

- Macro `wal/treesit` is now part of `harpoon`.
- Macro `harpoon` was greatly refactored again to account for
  `treesit`.
- The `transient` `harpoon` dispatches were renamed from
  `wal/<mode>-dispatch` to `mode-captain`.
- Macro `parallel` now differentiates by numeric and "normal" prefix
  argument. This means calling the B command requires `C-u 0` instead
  of `C-u`. This allows for both commands to consume the argument.
  Keyword `:universalize` allows converting numeric to non-numeric if
  the called command expects `'(4)` over `4` etc.
- `project` ignores `.ccls-cache`.
- Most `which-key` replacements that are not prefixes were removed.
- Transients for `winner` and `smerge` were replaced by using (and
  adding) `repeat-mode` maps.
- `display-buffer` utility functions were reduced to two cases:
  `wal/display-buffer-same-place-or-{faraway,nearby}`.
- The order of `wal/key-reach` has changed, pushing `colonel` and
  `general` to `[` and `]`.
- Non-`captain` dispatches no longer use custom transient.

### Removed

- Package `jsonian` in favor of using built-in `js-json-mode`.
- Package `emojify` in favor of relying on built-in emoji support.
- Several `parallel` uses. The transients were moved to `wal/major`.

## Fixed

- Template for `docstring` now indents the other lines.

## [v1.10.3]

Piece of junk.

### Added

- Buffer source for `consult` to narrow to (contributing) agenda
  buffers.
- Support for `treesit` in `harpoon` and using new macro
  `wal/treesit`. Currently only used for `js-mode` as a test.
- Ligatures can now be set through `harpoon` using keyword `:ligatures`.
- `project-switch-action` to open project root; command to compile
  project.
- Additional functions to be called are now part of `harpoon` using
  keywords `:functions` so that they're only called if they're bound.

### Changed

- User-prefixed `i` both stores and loads using `consult` again.
- `wal/misc-map` was renamed to `wal/triple-minus-mode`.
- `wal/java-mode-dispatch` now uses `transient-switches` instead of
  completion to switch mode and scope.
- `log4j-mode` is no longer enabled for all `*.log` files.
- Macro `wal/hook` was renamed to `harpoon` and refactored into
  meaningful sub-macros.
- Macro `wal/define-expansion-pack` was renamed to `junk-expand` and
  all underlying functionality to `junk-*`.
- Macro `wal/univ` was renamed to `parallel`. The function it creates
  now no longer just prefixes the first function's name with
  `wal/univ-*` but instead constructs `<a>||<b>` out of both names.
  `parallel` now determines callee on whether the prefix argument is
  numeric or not, which allows both commands to have a meaningful
  value.
- Function `wal/l` now toggles dedication.
- Tasks in `org/tasks` are filtered for `dashboard`.

### Removed

- Custom variable `wal/idle-delay` in favor of using distinct values
  for distinct cases.

### Fixed

- `popper` bindings are now bound in global map.
- `consult-customize` sets `:preview-key` using string (seems a recent
  change).
- Same fix for `embark`.
- `wal/dap-stopped` now calls `wal/dap` instead of missing
  `dap/hydra/body`, setting `transient-show-popup` to `t` so it's
  immediately shown.
- `wal/project-command` makes sure `project-vc-name` doesn't mess with
  any output.
- `wal/maybe-org-roam-refile` was renamed to `wal/org-refile` and now
  listens for numeric prefix argument to skip `org-roam`-specific
  behavior.

## [v1.10.2]

In cold boot.

### Added

- Scripts to simulate a cold boot (where nothing has been installed)
  in `setup/setup-cold-boot.sh` and `setup/wal-setup-cold-boot.el`.
- Function `wal/persist-scratch` now persists the content of all
  scratch buffers.
- Function `wal/bootstrap-config` now accepts second optional argument
  `cold-boot` to mimic a cold boot by setting `package-user-dir` to a
  temporary directory.
- The handler for `use-package` keyword `:hook` is now advised to only
  handle if the respective is either installed or built-in.

### Changed

- Idle delay was doubled.
- Org `transient` now binds `org-info-find-node`.
- The `org` transient now allow sorting from headings; prefixes were
  changed to free `s` (subtrees now use `t` and timestamps `.`).

### Removed

- Package `python-pytest` since it uses `projectile` under the hood
  which causes problems.
- `wal/dwim-key` in favor of unique bindings.
- Package `hydra` in favor of using `transient`. All hydras with the
  exception of `wal/fly` (which I don't use) have been replaced.

### Fixed

- Cold booting the configuration should now work.
- `wal/project-magit-status` now will not run if the (sub-)project is
  not version-controlled.
- LSP should no longer complain about missing `yasnippet`.
- `wal/load-theme` now captures errors if loading a theme fails.
- `ligature-set-ligature` is no longer called directly to avoid issues
  on a cold boot.
- `wal-prelude` now emits messages about what's going on and the error
  handling was moved to `wal/load-config` to stop after a single
  error.
- `wal/site-lisp` not existing should no longer cause issues.

## [v1.10.1]

Wooden dinosaur.

### Added

- Regular expression for `js` to follow errors in jest output.
- `wal/hook` now accepts key `shallow` to not meddle with indentation.
- Compilations buffers now filter ANSI colors.
- Package `iedit`.
- `project-switch-commands` `wal/project-consult-buffer` and
  `wal/project-magit-status`.
- Project commands now have a per-project history.

### Changed

- `slime` no longer requires `sbcl` executable to exist.
- Most custom additional bindings were removed from built-in maps. QoL
  bindings were kept.
- Package sections that have custom utility are now subdivided by
  Utility and Configuration sections.
- Setting `corfu-auto-{delay,prefix}` is now no longer tied to
  `wal/lsp`.
- User-prefixed `y` now loads from register.

### Removed

- Package `jest` because of its `projectile` dependency leading to
  issues.
- Calling `org-agenda-list` no longer stores a window configuration.
- Several `find` commands (and their bindings in config transient).

### Fixed

- `pixel-scroll-precision-mode` now only activated for 29+.

## [v1.10.0]

Plan vanilla.

### Added

- Commands `wal/project-test` and `wal/project-install` that work like
  `project-compile` but store the previously used command in a hash
  table using the project as the key.
- More robust mocking tools.
- Full test coverage.
- Function `wal/avy-goto-word` now goes to word in line first. Called
  with `C-u` goes to word in buffer, called with multiple `C-u` goes
  to word anywhere.
- `wal/scratch-buffer` now allows session creation/selection like
  `eshell`.
- Package `log4j-mode` as part of new `java` expansion pack.
- Function `wal/project-rg` as a replacement for
  `project-find-regexp`.

### Changed

- `use-package-always-ensure` is no longer set to `t` by default.
  Instead, new flag `wal/flag-ensure` can be set with `--ensure`.
- `corfu` is now turned on in `eshell`.
- The map for `wal/general` was flattened. Both `eshell` and `eww` can
  be accessed directly.
- Exit command `wal/delete-edit-or-kill` is now bound to `C-x C-c`
  (the default binding).
- `org-src` now splits window vertically when editing source code.

### Removed

- Package `projectile` in favor of using built-in `project`.
- Custom flag `wal/flag-deny`.
- Packages `esh-autosuggest` and `eshell-prompt-extras`.
- Binding of command `restart-emacs`.
- `hydra` to scroll other window.
- `paredit` in favor of `puni`.

### Fixed

- Coverage calculation now calculates covered / relevant lines.
- Coverage can be calculated for buffers with count greater than 9.

## [v1.9.4]

Sweet toot.

### Added

- Around 120 additional tests for (almost) all packages. There are no
  tests for the settings package as all it does is set things.
- `project-compile` is now advised to only save buffers that are part
  of the current project.
- Command `wal/dired-config-tests` to open the tests directory with
  Dired.

### Changed

- Tests, test helper and `cask` were moved out of the configuration
  into project root.
- `consult-flycheck` is now bound in `wal/consult-map`.
- User-prefixed `u` now binds new function `wal/avy-goto-word` that
  replaces `wal/avy-goto-word-in-line`.
- Packages are now loaded from `wal/bootstrap-config` which is part of
  `wal-prelude` instead of doing it in `wal` package.
- Various sections from `README.org` now have their own document under
  `docs`.
- `wal/consult-line` now uses `thing-at-point` unless called with
  `C-u`.

### Removed

- `lispy` in favor of `paredit`.
- Custom minor mode `wal/config-mode` that was used for editing the
  config (the functionality is now provided by `wal/config-dispatch`).

### Fixed

- Using `project` over `projectile` should now work with `wal-line`.

## [v1.9.3]

Time killer.

### Added

- Command `wal/kill-some-popups` to do just that.
- Template function `wal/tempel-comment` using `c` used in new
  templates.
- List function `wal/list-from` to create a list using an element as
  its first element.
- New command `wal/kwim` to kill forward or whole line depending on
  the position of point in the line using `mwim`; it is now bound to
  `C-k`.
- New command `wal/org-clock-take-note` to take a note for the
  currently clocked task. Bound to `t` in `wal/misc-map`.
- Binds `H-|` to `popper-cycle`.

### Changed

- `wal/avy-goto-line` now uses `beginning-of-line-text` when going to
  beginning of line.
- Command `popper-toggle-latest` is now bound to user-prefixed `\`.
- User-prefixed `]` no longer binds `popper-cycle` (see below).
- Prefix for `tempel` is now `,,` to avoid clashes in languages that
  use `>`.
- All consult bindings were moved to renamed keymap `wal/consult-map`
  that uses `wal/consult-key` for its binding (`[` by default). This
  key is also used in the `rg` and `lsp` transients.
- Dispatch `org-roam` was moved to user-prefixed `]`.
- User-prefixed `i` now loads and stores register using `wal/univ`.
- The `tempel-path` is extended by templates provided in
  `templates/tempel.eld`.

### Removed

- Package `mode-line-bell`.
- `C-;` is no longer bound in favor of using `C-x C-;`.
- Binding `M-o` is no longer bound to `wal/other-window`; this command
  now overrides `other-window` by binding `C-x o`.
- The window splitter movement functions `wal/edge-*` as well as the
  `hydra` that bound them.
- Package `dimmer`.
- Package `drag-stuff`.
- Package `highlight-quoted` (lisp extra).
- Package `highlight-number`.

### Fixed

- `multiple-cursors` now disables (and re-enables) `corfu-mode` when
  entering/exiting.

## [v1.9.2]

Good trim.

### Added

- Function `wal/push-mark` that pushes a mark without activating it.
  Bound to `<SPC>` in `lieutenant`.
- Command `org-agenda-list` is now advised to find the tasks directory
  and store the window configuration using custom variable
  `wal/org-agenda-register-char`.
- Command `wal/kill-some-file-buffers` to run `kill-some-buffers` on
  buffers associated with files. Bound to `b` in `wal/misc-map`.
- Function `wal/java-test-dwim` that uses `transient` switches to call
  the right elisp command.
- Package `tempel`, replacing `yasnippet`.

### Changed

- User-prefixed `i` is now bound to `consult-register-load`;
  `consult-register` is still available via `ctl-x-r-map`.
- Function `wal/message-in-a-bottle` now uses the blue whale as a
  default (although any other string may optionally be passed.
- Font functions now display the current font height/family.
- `wal/org-roam-dispatch` is now bound to user-prefixed `y`.
- Help-like buffers are now displayed using `wdb/pop-up` instead of
  `wdb/direction`. Some side-buffers are no longer targetable by
  `other-window`.
- `popper` no longer groups.
- `transient` for config prefixes checkers with `c`.
- `wal/config-ascii-whale` uses different border thicknesses for
  different whales.
- Messages buffer is now using `wdb/pop-up` and is considered a
  `popper` pop-up.
- `help-mode` buffers are no longer pop-ups.
- The bindings for commands from the `consult` package have changed.
  The main change is that `c` is used instead of `u` in foreign maps.
- User-prefixed `u` now binds `consult-register-store`.
- `org-roam` is now bound in `major` to `r`; `verb` to `v`.
- User-prefixed `l` now calls `wal/avy-goto-line` that goes to end by
  default and to beginning if called with `C-u`. Those two actions are
  no longer bound in `goto-map`.

### Removed

- Package `yasnippet` was removed in favor of `tempel`.
- Package `vundo`.
- Package `org-transclusion`.
- Package `crux`.
- Package `org-bullets`.
- Package `display-wttr`.

## [v1.9.1]

Very cash money

### Added

- New ASCII blue whale as the default for the config animation; new
  custom variable `wal/config-ascii-whale` can be set to `cachalot` to
  get the old one.

### Changed

- `wal/lighthouse` is now bound to `C-c p`.
- `wal/agenda` was removed in favor of using the dispatch.
- Default value for `wal/idle-delay` was greatly reduced again to
  **0.8**.
- `dap-mode` now shows locals and REPL; both buffers' display has been
  overridden. It also stores and reloads window configurations and
  re-displays the custom `hydra` when hitting a breakpoint.
- Consult functions are now defined in own map bound to `u` in
  `lieutenant`.
- Most of the sink of `major` was moved to `general` as they're
  built-in and relate to the editor.
- Consult functions are now defined in own map bound to `u` in
  `lieutenant`.
- `completion-at-point` is now bound globally using `C-M-i` (like in
  other maps).
- The blue whale is also used for `wal-line`.

### Fixed

- Function `pet-find-executable` is advised to return the argument it
  was called with if it returns `nil`.
- Adds new function `wal/aw-delete-other-windows` to
  `delete-other-windows` while ignoring window parameters.

## [v1.9.0]

Vague novelty.

### Added

- Package `lsp-sonarlint` as an LSP expansion pack.
- Custom variable `wal/dwim-key` that is used in `consult` to preview,
  in `transient` to show and in `corfu` to `corfu-quick-complete`.
- `dashboard` is now also shown for daemon.
- `dashboard` truncates path beginnings.
- New macro `setq-unless` to only set a variable if it is not already
  set as an alternative to mirror `defcustom` variables.
- New function `wal/kmacro` to start/stop recording; bound to `k` in
  `lieutenant`.

### Changed

- Default value for `wal/idle-delay` was increased to **1.6**.
- Customized leading char face for `ace-window` to be bigger and
  inherit `mode-line-emphasis`.
- Ligatures are now declared in individual packages.
- External package no longer use `C-c` prefix. Exceptions are
  functions using `consult` wrappers. They were mostly moved to
  `major`.
- Leader key `colonel` is now responsible for config-related actions.
- Map `wal/fly` is now mapped to user-prefixed `f` and is an amaranth
  `hydra`.
- User-prefixed `i` now runs `consult-register`.
- `i` in `lieutenant` now binds `ctl-x-r-map` (register and
  rectangle).
- `hydra`s use `lv` again.

### Removed

- Package `perspective` has been mothballed in favor of using
  registers.
- Various `defcustom` variables were removed or made into local
  variables.
- Dailies are no longer part of the agenda.
- User-prefixed `y` no longer expands `yasnippet`; instead the default
  tab expansion was re-enabled.

## [v1.8.1]

Digital proxemics.

### Added

- Macro `wal/transient-define-prefix-once` to ensure that
  re-evaluating macro will not re-define the transient (unless
  variable `wal/transient-may-redefine` is `t`)

### Changed

- Leaders have switched places again to have rising ranks mirror
  rising responsibility. The custom variables have been replaced by
  singular variable `wal/key-reach` that holds a list of keys ordered
  by how reachable they are.
- Leader `colonel` is currently out of service again; its bindings
  have been folded into `major`; this required some bindings to change
  (`vundo` now uses `u`, `winner` uses `w`).
- Leader `major` now has a sink again.
- `hydra` is now displayed in the minibuffer.
- `hydra`s have been reworked to only be on a single line.
- `flycheck` error list now uses `wdb/side` to show at the bottom
  instead.
- `scroll-{up,down}-command` are now bound in `wal/fly` using keys `,`
  and `.`.
- `wal/fly` no longer binds `avy` commands; instead the `goto-map` is
  now again bound to user-prefixed `l`; the command is now bound to
  user-prefixed `'`.
- `hydra` for `text-resize` now also binds global adjustment; the
  reset also resets global one.

### Fixed

- Flag capturing in **Settings** section was moved up to make sure
  flags `doctor` and `deny` are captured before `use-package` is
  configured.

## [v1.8.0]

From hero to zero.

### Added

- `iedit-mode` is now bound in `colonel` using `i`.
- Command `wal/vterm-run` to quickly run and display command executed
  in a `vterm` buffer.
- Package `org-habit` now has a configuration.
- New parameter `lieutenant` in `wal/hook` that will bind function
  `wal/<mode-name>-dispatch` to the `lieutenant` key. This now the
  default access to major-mode specific `transient`s.
- `org-super-agenda` now adds a new-line after the final group.
- `graphql` as a Web extra (implicit also `graphql-mode`).
- Command `wal/prettier-refresh` to do just that.
- New macro `wal/when-ready` to execute a body using appropriate hook
  after start-up.
- Package `jest` as a `javascript` extra.
- `xah-fly-key`-like `hydra` to move, jump and delete.
- Function `wal/advise-many` to do just that.

### Changed

- `popper` groups by `perspective` again.
- `projectile-per-project-compilation-buffer` is set to `t`.
- `css-mode` now uses `lsp-mode`; if `stylelint` executable is found,
  that checker is explicitly selected.
- `corfu-auto-prefix` is now 2 and `corfu-auto-delay` is 0.2 for
  `js-mode`-derived modes when using `lsp-mode`.
- `lispy` is no longer activated immediately, it is instead bound in
  `transient`.
- Leader keys `captain` and `major` have been swapped.
- The default keys for the leader keys has changed.
- `popper` now uses user-prefixed keys `[` and `]` instead of being
  bound in (now) `major`.

### Removed

- Bindings in `lieutenant` were moved to `C-c *` bindings.
- Macro `wal/major!` has been removed as it's obsoleted by new
  `lieutenant` bindings.
- Packages `js2-mode` and `rjsx-mode` were removed in favor of using
  built-in `js-mode`.
- Many `defcustom` variables that weren't particularly useful.

### Fixed

- Command `wal/avy-action-zip-to-char` is now called
  `avy-action-zip-to-char` to not have a garbled help text.
- Commands `dap-java-{debug,run}-test-{class,method}` are now advised
  to use `bash` shell as `fish` seems to garble the class paths.

## [v1.7.14]

Cooled beans.

### Added

- Function `wal/kill-dired-buffers-in-perspective` to do just that.
- Configuration for the XML language server in `lsp-mode`.
- Binds `lsp-javascript-rename-file` in transient for `js2-mode`.
- New custom variable `wal/hidpi` that signals whether the screen is
  HiDPI. Setting this to true currently just increases the fringe
  width.
- Package `jenkinsfile-mode` as an expansion pack.
- New option `:prog-like` to `wal/hook` macro. If set to `t`
  `prog-like-hook` hooks will be run. These include `yas-minor-mode`,
  `drag-stuff-mode` and similar enhancing modes.

### Changed

- Command to find dailies directory is now bound in `org-roam`'s
  transient.
- The recipes for using hyper key were switched since the less risky
  one indeed does work.
- Command `wal/agenda` now stores and tries to restore window
  configuration that is created on first call; can be forced to skip
  if called with `universal-argument`.
- Variable `dired-kill-when-opening-new-dired-buffer` is set to `t`
  again.
- Variable `wal/modern-emacs` was replaced by function
  `wal/modern-emacs-p`.
- The ligatures for `html-mode` were improved and copied for
  `nxml-mode`.
- Command `consult-ripgrep` is now available as a suffix of `rg-menu`
  `transient` using key `u`.
- All `transient`s have been improved by moving external suffixes to
  the respective packages.

### Fixed

- Function `org-store-log-note` is now advised to remove agenda
  buffers from the perspective to avoid cluttering.
- Function `wal/insert-after` check if item is already in list before
  inserting.

### Removed

- The `transient`s for register, yas, consult, perspective, and
  projectile.

## [v1.7.13]

Give them the boot.

### Added

- Macro `wal/on-boot` was added to only evaluate a body during
  bootstrapping through `wal/bootstrap`, controlled by variable
  `wal/booting` set in `wal/prelude`.
- Sub-packages that have side-effect code now use this macro.
- Function `wal/kill-async-process-buffers` to do just that; used for
  coverage check.
- Function `wal/matches-in-string` to collect all regex matches in a
  string.
- Various tests to increase coverage; tests for `wal-look` and
  `wal-fonts`.
- Adds `use-package` keyword `:wal-bind-keymap` to do just that.
- User-prefixed `l` now uses this feature to bind `goto-map`.
- New custom variable `wal/prefer-rjsx` that is used to hook into
  `js2-mode` and allows switching to `rjsx-mode` for plain JS files.
- Remove `M-o` binding from `mhtml-mode`.
- Files with ending `.tsx` now also load `typescript-mode`.
- Built-in packages `mhtml-mode` and `nxml-mode` now have a
  configuration.

### Changed

- Function `wal/async-process` now takes a fourth optional argument to
  suffix the buffer created.
- The test helper section was moved to **Additional Package Files**.
- Function `wal/check-coverage` now calculates the overall coverage.
- The coverage status was moved to section **Package-Like**.
- Directory `wal/site-lisp-directory` is itself added to the
  `load-path` again.
- User-prefixed `h` no calls `projectile-find-file` to free
  user-prefixed `l`.
- Various bindings in `goto-map` were added for `avy`.
- Indentation functions no longer call `hack-local-variables`; this is
  now done by `wal/hook` macro.
- History size for `vterm` was increased tenfold.
- Custom `hydra` for `dap-mode` now also bind `dap-disconnect` and
  `dap-ui-repl`.
- Switches back to `traditional` way of displaying `magit`.

### Fixed

- Package `use-package` is now required after it could have been installed.
- Adding MELPA and requiring `package` now is odne in the **External
  Packages** section.
- Various `transient`s no longer reference potentially undefined
  commands. They now instead use custom suffixes.

### Removed

- `walled-mode` was replaced by function `wal/l` that serves the same
  purpose but is more testable.
- Various superfluous footnotes.
- `projectile-maybe-read-command` is no longer advised since
  `universal-argument` is swallowed by `transient`.
- No longer fixes the `jdtls` version.

## [v1.7.12]

Covert operation.

### Added

- Package `wal-prelude` now provides all functionality to bootstrap
  the configuration. Both the provided `init.el` template and
  `wal-setup-ci.el` use it.
- A Cask file was added to anticipate the package-ification of the
  config and to run tests.
- Tests were added for functions moved out of `wal-settings`.
- New package `wal-external` was added to hold the code to install the
  core packages as well as the `quelpa` configuration.
- The tests are now covered using `undercover`; this is done in the CI
  env and can be done locally using new function `wal/check-coverage`.

### Changed

- All `use-package` definitions now use a more relaxed style, leaving
  a blank space between sections. Style-guide was updated as well.
- Guideline for variable definitions changed. Values should always
  start on the first line.
- `isearch-lazy-count` is now `t`.
- Variable `wal/experimental` was renamed to `wal/modern-emacs`.
- The tests were moved from `test/` to the README configuration.
- Functions were moved out of `wal-settings` to `wal-func`.
- Settings were moved out of `wal-func` to `wal-settings`.
- The style-guide and cheat-sheet were moved to `docs/`.

### Fixed

- Function `wal/flycheck-file` kills buffers even when there are no
  errors.

### Removed

- The legacy Org files were discarded.

## [v1.7.11]

Action-backed.

### Added

- New flag `--deny` to disable setting `use-package-always-ensure` to
  `t`.
- Adds GitHub CI configuration to run the tests. This uses new custom
  file `wal-setup-ci` to tangle the config and set the package path
  using `GITHUB_WORKSPACE` environment variable.
- Tests for `wal/truncate`, `wal/reset-to-standard` and
  `wal/dead-shell-p`.
- Package `lsp-pyright`; extra for Python expansion pack.
- Package `python-black`, available through `transient`.
- Macro `wal/hook` now allows setting additional ignores for
  `lsp-file-watch-ignored-directories` using key `:lsp-ignores`.
- Function `wal/in-python-project-p` to check just that.
- Function `wal/lsp-pyright-install-stubs` to do just that.
- Added function from `corfu` README to enable it in minibuffer.
- Adds package `pet` and uses it to set up virtual environment paths.
- Functions to view this changelog and see the diff range since latest
  tag added to the editing transient.
- Function `wal/set-cursor-type` to do just that, bound in `general`.
- Custom `wal/consult-line` to use `thing-at-point` if called with
  `C-u`.

### Changed

- Function `wal/dead-shell-p` was moved to `wal-func`.
- `C-c a` now switches to initial perspective, opens the tasks
  directory and then opens the agenda in one go using new function
  `wal/agenda`. Default `org-agenda` is no longer advised to skip the
  dispatch.
- Packages are now responsible for setting the virtual environment
  path.
- Macro `wal/lang-hook` is now called `wal/hook` and used for
  non-language major modes.
- Package `lispy` was customized and now respects
  `multiple-cursors-mode`.
- Function `wal/capture-flag` is now a macro.

### Removed

- Package `org-sticky-header` has been mothballed in favor of new
  segment in `wal-line`.
- Package `ctrlf` has been mothballed in favor using `consult-line`.
- Package `beacon` has been mothballed in favor of using `pulse`.
- The `gc-cons-threshold` is no longer meddled with when setting up
  the minibuffer.

### Fixed

- Function `wal/truncate` now defaults to a length of 8 if `max-len`
  argument isn't provided.
- Custom `completion-style` was removed as it was buggy.
- Positioning of whale animation while editing the config no longer
  subtracts the fringe width from the horizontal offset.
- Variable `read-process-output-max` is set to 1 MB as this is the
  default max for `/proc/sys/fs/pipe-max-size`. The associated custom
  variable was removed.

## [1.7.10]

It gets lispier.

### Added

- First test suite for testing list utility in `wal-func`.
- New function `wal/flycheck-file` to do just that.
- New function `wal/flycheck-config-packages` to check all config
  packages using function above.
- The style guide now requires that package configuration code blocks
  are structured and ideally have some comments explaining non-obvious
  statements.
- New package `lispy` for better paren traversal.

### Changed

- All used advice combinators now follow a template (detailed in
  style-guide) instead of just prefixing the advised function with
  `wal/advise` to the advised.
- Binding in `wal/general` to quit now calls `delete-frame` in daemon
  mode.
- All utility functions editing lists now expect a symbol as their
  first argument.
- Settings are set before activating global modes (needed for
  `recentf` fix below).
- Package `vundo` is now bound in `major`. Function `wal/transparency`
  is now bound in `general`.
- Helper function `wal/display-buffer-condition` now uses an alist to
  provide major-mode conditional.
- The ASCII whale animation is now bigger and slightly offset; the
  code for it now is in the new relaxed, unfurled style.
- The two existing flags `mini` and `doctor` now use the prefix
  `wal/flag`.

### Fixed

- Package `org-roam` loads on first call of `wal/org-roam` again;
  problem was that the `:config` was referencing custom variables not
  yet set.
- Sets `recentf-filename-handlers` to nil so that
  `consult--source-project-recent-file` works that doesn't respect
  abbreviation.
- Agenda files now get the expanded directory, not the symbol.
- Custom exit function should work in `-nw` Emacs sessions.
- The hook for `python-mode` no longer expects `poetry` to be present
  and the function to activate the virtual environment no longer
  pretends to work for non-poetry projects (this addresses a change in
  that package).
- A `pyvenv` hook now takes care of setting the correct `lsp-pylsp`
  variables.
- Python packages are no longer demanded.

## [1.7.9] - 2021-08-20

Hunt and edit.

### Added

- New custom flag `--mini` that will do the same thing as setting
  `wal/minimal` (only loading packages that don't have `:wal-ways` set
  to `nil`).
- Macro `wal-define-expansion-pack` now also accepts passing MELPA
  recipes using key `:recipes`. Recipes and packages are treated
  equally in the `marginalia` annotation.
- Command `helpful-kill-buffers` is not bound in `help-map` to `C-x`.
- Configuration and expansion pack for `jakt-mode`.
- Sets `org-cycle-separator-lines` to 1 to avoid cramped look when
  headlines are collapsed.
- Docker container pop-ups now have a regex for
  `popper-echo-transform`; the `docker-compose` one was improved as
  well.
- Speed command for refiling (`w`) in an `org-roam` buffer now uses
  `org-roam-refile` unless called with `universal-argument`.
- Binds `org-toggle-timestamp-type` in the mode's `transient`.
- Capture template for `org-roam` dailies now sets current time
  (inactive) and task as props.
- Package `ef-themes`, configuring it to not mess with org headings.
- Package `corfu` now uses custom `orderless` dispatch (taken from
  official docs) when editing `lsp-mode` buffer.
- `recentf-mode` is enabled explicitly.
- Command `consult-ripgrep` is now bound to `C-c g`; it also uses new
  advice `wal/with-big-vertico` to increase `vertico-count`.
- List helper function `wal/insert-after` to insert an element after a
  preceding item. This also replaces function
  `wal/insert-use-package-keyword`. It is also used to add custom
  `consult` source for Dired earlier.
- Moving items up and down now bound in `markdown-mode` with `M-{<up>,
  <down>}`.

### Changed

- The default key for the `transient` in `major` is `.` again since
  the sink is gone.
- Bindings using `wal/major!` now set `which-key` replacement string
  "MAJOR!".
- Custom `hydra`s are bound passing `which-key` replacement string to
  make them more readable (for example to have `text-scale` instead of
  `wal/text-scale/body`).
- Function `wal/flyspell` also just uses `flyspell` for replacement.
- Command `dictionary-lookup-definition` is now bound using key `f`.
- Many of the pop-ups are now using `wdb/side` which puts them at the
  bottom.
- Pop-ups are now grouped using `projectile` or `project` instead of
  `perspective` which often fails.
- Compilation buffers are now shown in a pop-up. So are `rg` buffers.
- The `regexp-builder` buffer is filtered by `consult`.
- Since the package is now available from MELPA, `ligature` no longer
  uses `quelpa`.
- The prefix for `org-roam` was changed back to `H-<SPC>`.
- Only the dailies of `org-roam` are now added to the agenda files.
- Instead of binding `C-c C-c` and `C-c C-k` to `server-edit` (and
  `abort`), the default quit action is now `wal/edit-or-kill` that
  aims to make the right call.
- `lsp-ui-sideline-mode` is no longer enabled by default.

### Removed

- Function `wal/install-packages` no longer has key `:on-done` to
  print a message after completion. This is now expected to be done by
  the caller.
- The `org-roam` capture template setting active timestamps was
  removed; as was the advice to select the default template when going
  to a date.

### Fixed

- Package `typo-mode` is deferred after `text-mode`.
- Default `org-capture` is bound to `o` in `org-roam` `transient`.
- Adds missing `defvar` declarations to allow for dynamic binding.
- Macro `wal/lang-hook` now consumes passed doc string by producing a
  full function definition before adding the hook.
- Package `gdscript-mode` is now called that in the expansion pack,
  not `gdscript-godot`.
- Clocking in using `consult` now switches to the initial perspective
  beforehand to avoid cluttering the current one with unrelated
  buffers.

## [1.7.8] - 2021-08-13

Captain my captain.

### Added

- Added `gdscript-mode` and configuration.
- If a buffer was requested from a server client `C-c C-c` will call
  `server-edit` and `C-c C-k` will call `server-edit-abort`.
- Added package `base16-theme`.
- All packages now set `lexical-binding` to `t`.
- Added functions from `consult` wiki to clock in on agenda items and
  to use `thing-at-point` for `consult-line`.

### Changed

- `M-q` is now bound to `vertico-quick-exit` in `vertico` map;
  `quick-insert` is bound to `C-q`.
- `M-q` is now bound to `coruf-quick-exit` in `corfu` map;
  `quick-complete` is bound to `C-q`.
- Bindings in `colonel` have been re-assigned across it and its
  re-added mirroring `sink`.
- Responsibilities for `major`, `captain` and `lieutenant` have
  changed. Buffer-related actions, whether from `major-` or
  `minor-mode` are bound in `major`; global (or perspective-wide)
  actions in `captain`. Commands bound to `lieutenant` have been moved
  to `major`; `lieutenant` is now responsible for command maps. The
  `transient`s for `major-mode`s are now bound in `major` using new
  variable `wal/major!-key` (default is `m`) to allow for the definer
  to still have a sink.
- The `transient` for `consult` was slightly improved, no longer
  binding already bound commands and instead binding `apropos`,
  `keep-lines` and opening files externally.
- `C-c c` now does a `completion-at-point`.
- Commands `popper-toggle-latest` and `wal/org-roam` were moved to
  `captain`.
- Macro `wal/major!` now uses `H-. m`.
- The effect of `goggles` is more pronounced.
- Zoning is now bound to `0` in `captain`.
- Bindings that are not a built-in sub-program were removed from
  `general-sink`.

### Fixed

- Commands bound in `transient` for `org-mode` are more context-aware.
- Fixes `corfu` and `orderless` actually working with `lsp-mode`.
- Adapts advice for `vertico-directory` to work again after advised
  function was removed.
- Makes `projectile` not pass potentially missing switch to `fd`.
- Loading of `mu4e` should work again by removing it from the
  expansion pack (which would check if it was installed using
  `package`). The command is now bound to `C-c m`.
- Deleting window with `ace-window` now uses `x` (previously `k` was
  bound which would be used to switch).
- Package `dumb-jump` now uses `rg`.

### Removed

- Package `restart-emacs` was mothballed in favor of newly built-in
  command of the same name.
- Package `imenu-list` was mothballed.

## [1.7.7] - 2021-08-03

Viz major.

### Added

- Several `transient`s for various `major-mode`s and libraries. The
  package is now demanded.
- New macro `wal/major!` to more easily bind to `H-. .`.
- Commands bound to `wal/colonel` are now described.

### Changed

- There are no bindings in `wal/major` anymore that belong to a
  particular `major-mode`, and it no longer has a sink. Instead the
  `wal/major-key` calls the mode's `transient` (see above).
- The dispatch for this config is now bound to `C-c 9` (in its
  buffer).
- Packages `projectile`, `perspective` and `yasnippet` no longer use
  `wal/univ` macro. The transients are now bound in `wal/captain`
  using the same key.
- Package `lsp-mode` no longer uses `H-h`, the transient is bound in
  `wal/captain` as well (using `l`).
- Command maps for `projectile`, `perspective` and `lsp-mode` are now
  bound using uppercase keys in `wal/captain`.
- Non-minor-mode command maps are bound using their keys.
- The `hydra` for `winner` as well as `hs-toggle-hiding` were moved to
  `captain`.
- Some `:defer` values were removed or reduced.

### Removed

- Package `explain-pause-mode` was mothballed.

### Fixed

- `general` binding for `kmacro` command map.
- Package configurations no longer use both `:demand` and `:commands`.

## [1.7.6] - 2021-07-28

Hook, line and syncer.

### Added

- Maps `org-next-visible-heading` (and `previous`) to `M-n` and `M-p`
  respectively.
- New function `wal/maybe-make-directory` to do just that.
- New macro `wal/lang-hook`. It controls indentation, fluff messages
  and whether `lsp-mode` needs to be turned on. This macro replaces
  (almost) all `wal/<mode>-hook` functions.
- `C-c s` now calls `consult-line`.
- The register dispatch has been greatly expanded and renamed.

### Changed

- The advice for `list-buffers--refresh` no longer filters if called
  with `C-u`.
- Function `wal/other-window` will now call `other-frame` if another
  frame exists.
- Disabling and enabling tabs now already takes care of hacking local
  variables appropriately (when disabling, only when called
  non-interactively).
- Use `M-q` for `vertico-quick` (like `corfu-quick`).
- The `transient` for `consult` now binds
  `wal/consult-ripgrep-ignored` again.
- Leader key `seargent` (`org-roam`) was removed. The package now has
  a `transient` that is bound to `C-c c`.
- The custom `beacon-blink` now uses `C-c b`.
- Finding daily file with `org-roam` is now advised to preselect the
  default template.
- `C-c r` now calls `transient` for register functions, no longer
  `completion-at-point`.
- Function `org-agenda` is now advised to not prompt and instead go to
  the day's agenda directly.

### Fixed

- Turning off `org-tree-slide` now only turns on modes that were
  actually turned off for it. `dimmer-mode` is now also turned off.
  `beacon-mode` is no longer turned on or off.

### Removed

- The unused `with-mode` keyword was removed from
  `wal/maybe-enable-tabs`.
- Leader key

## [1.7.5] - 2021-07-17

Hefty lefty.

### Added

- Function to write a `dap-mode` template into a JSON and save that as
  a file.
- Macro `wal/try` to do a safe `require` before executing body.
- Added `hydra` for `smerge-mode`.

### Changed

- `dap-mode`-Template for `debugpy` is now an interactive function.
- The `consult` dispatch is bound to `wal/univ`-ed `consult-buffer`
  again.
- `H-u` doubles for `C-u` now.
- Binds `yas-insert-snippet` in its `transient`.
- Adds `wal/univ` commands to `consult` and `projectile` configurations.
- Improves (and extends) the `transient` for `consult`.
- Binds `persp-forget-buffer` in its `transient`; the function
  definitions were shuffled a bit.
- Keymaps for `kmacro` and `bookmark` are now bound in `wal/captain`.
- Kubernetes is now user-prefixed (meaning the default is `H-8`).
- Moves non-letter user-prefixed bindings to use `C-c` instead. This
  was done to both relieve some pinky pressure and potentially free
  prefixed symbol keys.

### Fixed

- Several invocations of `H-u` call `universal-argument-more` now so
  it can be used like `C-u`. Previously only one argument was
  possible.

### Removed

- Package `perspective` is now always enabled, custom variable
  `wal/use-perspectives` was removed.
- Valid efforts are no longer set for `org-mode`.

## [1.7.4] - 2021-07-09

Perpetual transience.

### Added

- Replaced `json-mode` with `jsonian-mode`.
- Expansion pack extras can now be installed individually **and** in
  bulk.
- Going to clocked task bound in `goto-map` + "t".
- Explicit `transient` configuration.
- Binds `avy-goto-char` and `avy-goto-word-0` in `goto-map`.
- Binds more commands in the `transient` for `consult`.
- Creates `transient` maps for `projectile`, `perspective`,
  `yasnippet` and binds them to their keys using `wal/univ`.
- Creates `transient` for `verb` and its response buffers.

### Changed

- Response buffers for `verb` now use directional display.
- Improved bindings for `org-roam`, notably to bind capturing "today".
- Package `org-roam` is now an expansion itself.
- The templates used by `org-roam` have been slightly adapted; the one
  for daily files has an alternative one for adding an active
  timestamp.
- The expansion packs were slightly adapted.
- Package `yasnippet` was reverted to old hook-based configuration.
- Jumping to line and word was moved to `goto-map` using "l" and "w";
  function `wal/avy-jump` was renamed to `wal/avy-goto-word`.
- The freed keys ("j" and "l") are now used by `consult-buffer` and
  `projectile-` and `project-find-file` respectively.
- Package `projectile` uses "p" again for its prefixes.
- Package `lsp-mode` uses "l" again for its prefixes and "h" for the
  dispatch.
- Binding `:wal-bind` for "u" is bound to the dispatch for now.
- Some functions were moved to `wal-func`.
- Forces `compile-mode` to scroll.

### Removed

- Package `org-alert` was mothballed as it doesn't reliably work and
  freezes Emacs too much.

## [1.7.3] - 2021-06-26

Active pause.

### Added

- New variable `wal/active-theme` set during initialization. Function
  `consult-theme` updates it. The variable is read when calling new
  function `wal/load-active-theme` (also called during
  initialization).
- Functions `query-replace` are now bound in `wal/colonel` to "q" and
  "Q".
- Temporary fix to make `kubernetes--save-window-state` work.
- Temporary fix to require `org-indent` because some package uses its
  face without requiring the package.
- Binds toggling built-in package `hideshow`.
- New function `wal/avy-mark-region` to do just that, bound to
  `wal/colonel` + "r SPC".
- Sets `persp-modestring-short` if even the truncated perspective
  names go beyond new threshold `wal/perspective-shorten-after`
  (default is 30 characters).
- Adds `explain-pause-mode` using `quelpa` to have a history of why
  Emacs was slow.

### Changed

- Custom variable `wal/perspective-max-modeline-length` was renamed to
  `wal/perspective-truncate-after`.
- Custom command line flag `--doctor` not sets new variable
  `wal/doctor` which both controls `use-package` collecting statistics
  and turns on `explain-pause-mode`.
- Updated `commitlint` dependencies.
- Dispatch/command map for `bookmark` now uses "C-c m".
- Lints `commitlint` config with `prettier`.

### Removed

- Package `doom-modeline`.
- Package `golden-ratio`.
- Package `company` and `slime-company`.
- Package `highlight-indent-guide` (temporarily).
- Hack to fix duplicate logs in `lsp-dart`.
- Hack to fix `kubernetes` function to refresh overview (about to be
  fixed upstream).
- Hack to fix autoloads for `kubernetes-overview` (seemingly fixed
  upstream).

## [1.7.2] - 2021-06-12

Slug or snail.

### Added

- Warning emitted for non-GNU systems can now be ignored by setting
  `wal/ack-warnings` to `t`.
- Adds and configures packages `python-pytest`.
- Adds `org-speed` command "N" to log non-continuously (as in from
  now).
- Adds `ein` as a Python expansion pack extra.
- Adds `org-alert`.
- Configures `org-refile` to consider headlines up to level 3.

### Changed

- Added listing errors to `lsp-mode` transient.
- Improved inferior shell mappings for `python-mode`.
- Finding a file with projectile while not in a project is now advised
  to switch projects.
- Capture template for `org-roam` now uses slug with no date prefix.
- Commands from `projectile` are advised to only prompt if
  `universal-argument` is given.
- The `modus-themes` are now always loaded because of a customization
  quirk.
- Upgraded to latest `commitlint` version.

### Fixed

- Package `marginalia` is now required with errors ignored (on first
  start-up, the package won't be there).
- Expansion pack packages are now only loaded if installed.
- Advise function `kubernetes-utils--save-window-state` to use a
  function, not a macro so that it works (in Emacs 29).
- Disables `org-sticky-header-mode` in presentations.
- Borderless setting for `modus-themes` was fixed.
- Fixes `slime` requiring potentially missing `slime-company`.

## [1.7.1] - 2021-05-14

Less is less.

### Added

- Adds and configure package `imenu-list`.
- Adds and configures package `org-transclusion`.
- Adds and configures package `goggles`.

### Changed

- Uses `modify-all-frames-parameters` for setting transparency and
  also updates `wal/transparency`.
- Suggest using `.Xmodmap` file for hyper/caps switch.
- No longer uses the `consult` variants of yanking.
- The default `dap-mode` hydra is now bound in the smaller hydra.
- Offset for `fish-mode` is now 2.

### Fixed

- Remove `eshell-mode-map` mapping in `eshell` configuration (the map
  belongs to `esh-mode` but is no longer required).
- Advice for `persp-format-name` takes names shorter than the
  truncation into account.
- Always use copying for backup files to prevent problems with
  `lsp-mode` managed buffers.
- Custom `use-package` keyword `:wal-ways` should now work as intended
  (and not only if a different keyword was included in the state).
  Packages should now be automatically ensured again unless the
  evaluation of keyword, `wal/minimal` and `wal/minimal-exclude`
  resolves to `nil`.

## [1.7.0] - 2021-04-30

Locally sourced.

### Added

- Added `quelpa` so non-MELPA packages can be installed from GitHub.
- Added my mode-line package `wal-line` using `quelpa` recipe.
- Package `ligature` is now installed using `quelpa`.
- Upgraded `commitlint` to latest version.

### Removed

- Package `org-bullets` in favor of using `org-modern`.

### Fixed

- Icons set for `popper` and `org-sticky-header` now use
  `all-the-icons-*` instead of the `doom-modeline` equivalent to
  decouple functionality from `doom-modeline`.
