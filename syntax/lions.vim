" Vim syntax file
" Language:     lions
" Maintainer:   liujun
" Last Change:  Aug 8, 2020
" For bugs, patches and license go to https://github.com/lions-language/lions.vim

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Syntax definitions {{{1
" Basic keywords {{{2
syn keyword   lionsConditional match if else
syn keyword   lionsRepeat loop while
" `:syn match` must be used to prioritize highlighting `for` keyword.
syn match     lionsRepeat /\<for\>/
" Highlight `for` keyword in `impl ... for ... {}` statement. This line must
" be put after previous `syn match` line to overwrite it.
syn match     lionsKeyword /\%(\<impl\>.\+\)\@<=\<for\>/
syn keyword   lionsRepeat in
syn keyword   lionsTypedef type nextgroup=lionsIdentifier skipwhite skipempty
syn keyword   lionsStructure struct enum nextgroup=lionsIdentifier skipwhite skipempty
syn keyword   lionsUnion union nextgroup=lionsIdentifier skipwhite skipempty contained
syn match lionsUnionContextual /\<union\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*/ transparent contains=lionsUnion
syn keyword   lionsOperator    as
syn keyword   lionsExistential existential nextgroup=lionsTypedef skipwhite skipempty contained
syn match lionsExistentialContextual /\<existential\_s\+type/ transparent contains=lionsExistential,lionsTypedef

syn match     lionsAssert      "\<assert\(\w\)*!" contained
syn match     lionsPanic       "\<panic\(\w\)*!" contained
syn match     lionsAsync       "\<async\%(\s\|\n\)\@="
syn keyword   lionsKeyword     break
syn keyword   lionsKeyword     box
syn keyword   lionsKeyword     continue
syn keyword   lionsKeyword     crate
syn keyword   lionsKeyword     extern nextgroup=lionsExternCrate,lionsObsoleteExternMod skipwhite skipempty
syn keyword   lionsKeyword     fn func nextgroup=lionsFuncName skipwhite skipempty
syn keyword   lionsKeyword     impl let var
syn keyword   lionsKeyword     macro
syn keyword   lionsKeyword     pub nextgroup=lionsPubScope skipwhite skipempty
syn keyword   lionsKeyword     return
syn keyword   lionsKeyword     yield
syn keyword   lionsSuper       super
syn keyword   lionsKeyword     where
syn keyword   lionsUnsafeKeyword unsafe
syn keyword   lionsKeyword     use nextgroup=lionsModPath skipwhite skipempty
" FIXME: Scoped impl's name is also fallen in this category
syn keyword   lionsKeyword     mod trait nextgroup=lionsIdentifier skipwhite skipempty
syn keyword   lionsStorage     move mut ref static const
syn match     lionsDefault     /\<default\ze\_s\+\(impl\|fn\|type\|const\)\>/
syn keyword   lionsAwait       await
syn match     lionsKeyword     /\<try\>!\@!/ display

syn keyword lionsPubScopeCrate crate contained
syn match lionsPubScopeDelim /[()]/ contained
syn match lionsPubScope /([^()]*)/ contained contains=lionsPubScopeDelim,lionsPubScopeCrate,lionsSuper,lionsModPath,lionsModPathSep,lionsSelf transparent

syn keyword   lionsExternCrate crate contained nextgroup=lionsIdentifier,lionsExternCrateString skipwhite skipempty
" This is to get the `bar` part of `extern crate "foo" as bar;` highlighting.
syn match   lionsExternCrateString /".*"\_s*as/ contained nextgroup=lionsIdentifier skipwhite transparent skipempty contains=lionsString,lionsOperator
syn keyword   lionsObsoleteExternMod mod contained nextgroup=lionsIdentifier skipwhite skipempty

syn match     lionsIdentifier  contains=lionsIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match     lionsFuncName    "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

syn region lionsMacroRepeat matchgroup=lionsMacroRepeatDelimiters start="$(" end=")" contains=TOP nextgroup=lionsMacroRepeatCount
syn match lionsMacroRepeatCount ".\?[*+]" contained
syn match lionsMacroVariable "$\w\+"

" Reserved (but not yet used) keywords {{{2
syn keyword   lionsReservedKeyword become do priv typeof unsized abstract virtual final override

" Built-in types {{{2
syn keyword   lionsType        isize usize char bool u8 u16 u32 u64 u128 f32
syn keyword   lionsType        f64 i8 i16 i32 i64 i128 str Self string

" Things from the libstd v1 prelude (src/libstd/prelude/v1.rs) {{{2
" This section is just straight transformation of the contents of the prelude,
" to make it easy to update.

" Reexported core operators {{{3
syn keyword   lionsTrait       Copy Send Sized Sync
syn keyword   lionsTrait       Drop Fn FnMut FnOnce

" Reexported functions {{{3
" There’s no point in highlighting these; when one writes drop( or drop::< it
" gets the same highlighting anyway, and if someone writes `let drop = …;` we
" don’t really want *that* drop to be highlighted.
"syn keyword lionsFunction drop

" Reexported types and traits {{{3
syn keyword lionsTrait Box
syn keyword lionsTrait ToOwned
syn keyword lionsTrait Clone
syn keyword lionsTrait PartialEq PartialOrd Eq Ord
syn keyword lionsTrait AsRef AsMut Into From
syn keyword lionsTrait Default
syn keyword lionsTrait Iterator Extend IntoIterator
syn keyword lionsTrait DoubleEndedIterator ExactSizeIterator
syn keyword lionsEnum Option
syn keyword lionsEnumVariant Some None
syn keyword lionsEnum Result
syn keyword lionsEnumVariant Ok Err
syn keyword lionsTrait SliceConcatExt
syn keyword lionsTrait String ToString
syn keyword lionsTrait Vec

" Other syntax {{{2
syn keyword   lionsStrfmt      strfmt
syn keyword   lionsSelf        self
syn keyword   lionsBoolean     true false

" If foo::bar changes to foo.bar, change this ("::" to "\.").
" If foo::bar changes to Foo::bar, change this (first "\w" to "\u").
syn match     lionsModPath     "\w\(\w\)*::[^<]"he=e-3,me=e-3
syn match     lionsModPathSep  "::"

syn match     lionsFuncCall    "\w\(\w\)*("he=e-1,me=e-1
syn match     lionsFuncCall    "\w\(\w\)*::<"he=e-3,me=e-3 " foo::<T>();

" This is merely a convention; note also the use of [A-Z], restricting it to
" latin identifiers rather than the full Unicode uppercase. I have not used
" [:upper:] as it depends upon 'noignorecase'
"syn match     lionsCapsIdent    display "[A-Z]\w\(\w\)*"

syn match     lionsOperator     display "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)=\?"
" This one isn't *quite* right, as we could have binary-& with a reference
syn match     lionsSigil        display /&\s\+[&~@*][^)= \t\r\n]/he=e-1,me=e-1
syn match     lionsSigil        display /[&~@*][^)= \t\r\n]/he=e-1,me=e-1
" This isn't actually correct; a closure with no arguments can be `|| { }`.
" Last, because the & in && isn't a sigil
syn match     lionsOperator     display "&&\|||"
" This is lionsArrowCharacter rather than lionsArrow for the sake of matchparen,
" so it skips the ->; see http://stackoverflow.com/a/30309949 for details.
syn match     lionsArrowCharacter display "->"
syn match     lionsQuestionMark display "?\([a-zA-Z]\+\)\@!"

syn match     lionsMacro       '\w\(\w\)*!' contains=lionsAssert,lionsPanic
syn match     lionsMacro       '#\w\(\w\)*' contains=lionsAssert,lionsPanic

syn match     lionsEscapeError   display contained /\\./
syn match     lionsEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn match     lionsEscapeUnicode display contained /\\u{\%(\x_*\)\{1,6}}/
syn match     lionsStringContinuation display contained /\\\n\s*/
syn region    lionsString      matchgroup=lionsStringDelimiter start=+b"+ skip=+\\\\\|\\"+ end=+"+ contains=lionsEscape,lionsEscapeError,lionsStringContinuation
syn region    lionsString      matchgroup=lionsStringDelimiter start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=lionsEscape,lionsEscapeUnicode,lionsEscapeError,lionsStringContinuation,@Spell
syn region    lionsString      matchgroup=lionsStringDelimiter start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

" Match attributes with either arbitrary syntax or special highlighting for
" derives. We still highlight strings and comments inside of the attribute.
syn region    lionsAttribute   start="#!\?\[" end="\]" contains=@lionsAttributeContents,lionsAttributeParenthesizedParens,lionsAttributeParenthesizedCurly,lionsAttributeParenthesizedBrackets,lionsDerive
syn region    lionsAttributeParenthesizedParens matchgroup=lionsAttribute start="\w\%(\w\)*("rs=e end=")"re=s transparent contained contains=lionsAttributeBalancedParens,@lionsAttributeContents
syn region    lionsAttributeParenthesizedCurly matchgroup=lionsAttribute start="\w\%(\w\)*{"rs=e end="}"re=s transparent contained contains=lionsAttributeBalancedCurly,@lionsAttributeContents
syn region    lionsAttributeParenthesizedBrackets matchgroup=lionsAttribute start="\w\%(\w\)*\["rs=e end="\]"re=s transparent contained contains=lionsAttributeBalancedBrackets,@lionsAttributeContents
syn region    lionsAttributeBalancedParens matchgroup=lionsAttribute start="("rs=e end=")"re=s transparent contained contains=lionsAttributeBalancedParens,@lionsAttributeContents
syn region    lionsAttributeBalancedCurly matchgroup=lionsAttribute start="{"rs=e end="}"re=s transparent contained contains=lionsAttributeBalancedCurly,@lionsAttributeContents
syn region    lionsAttributeBalancedBrackets matchgroup=lionsAttribute start="\["rs=e end="\]"re=s transparent contained contains=lionsAttributeBalancedBrackets,@lionsAttributeContents
syn cluster   lionsAttributeContents contains=lionsString,lionsCommentLine,lionsCommentBlock,lionsCommentLineDocError,lionsCommentBlockDocError
syn region    lionsDerive      start="derive(" end=")" contained contains=lionsDeriveTrait
" This list comes from src/libsyntax/ext/deriving/mod.rs
" Some are deprecated (Encodable, Decodable) or to be removed after a new snapshot (Show).
syn keyword   lionsDeriveTrait contained Clone Hash RustcEncodable RustcDecodable Encodable Decodable PartialEq Eq PartialOrd Ord Rand Show Debug Default FromPrimitive Send Sync Copy

" dyn keyword: It's only a keyword when used inside a type expression, so
" we make effort here to highlight it only when Rust identifiers follow it
" (not minding the case of pre-2018 Rust where a path starting with :: can
" follow).
"
" This is so that uses of dyn variable names such as in 'let &dyn = &2'
" and 'let dyn = 2' will not get highlighted as a keyword.
syn match     lionsKeyword "\<dyn\ze\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)" contains=lionsDynKeyword
syn keyword   lionsDynKeyword  dyn contained

" Number literals
syn match     lionsDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     lionsHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     lionsOctNumber   display "\<0o[0-7_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     lionsBinNumber   display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="

" Special case for numbers of the form "1." which are float literals, unless followed by
" an identifier, which makes them integer literals with a method call or field access,
" or by another ".", which makes them integer literals followed by the ".." token.
" (This must go first so the others take precedence.)
syn match     lionsFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
syn match     lionsFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
syn match     lionsFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
syn match     lionsFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"

" For the benefit of delimitMate
syn region lionsLifetimeCandidate display start=/&'\%(\([^'\\]\|\\\(['nrt0\\\"]\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'\)\@!/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=lionsSigil,lionsLifetime
syn region lionsGenericRegion display start=/<\%('\|[^[:cntrl:][:space:][:punct:]]\)\@=')\S\@=/ end=/>/ contains=lionsGenericLifetimeCandidate
syn region lionsGenericLifetimeCandidate display start=/\%(<\|,\s*\)\@<='/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=lionsSigil,lionsLifetime

"lionsLifetime must appear before lionsCharacter, or chars will get the lifetime highlighting
syn match     lionsLifetime    display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match     lionsLabel       display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*:"
syn match     lionsLabel       display "\%(\<\%(break\|continue\)\s*\)\@<=\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match   lionsCharacterInvalid   display contained /b\?'\zs[\n\r\t']\ze'/
" The groups negated here add up to 0-255 but nothing else (they do not seem to go beyond ASCII).
syn match   lionsCharacterInvalidUnicode   display contained /b'\zs[^[:cntrl:][:graph:][:alnum:][:space:]]\ze'/
syn match   lionsCharacter   /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/ contains=lionsEscape,lionsEscapeError,lionsCharacterInvalid,lionsCharacterInvalidUnicode
syn match   lionsCharacter   /'\([^\\]\|\\\(.\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'/ contains=lionsEscape,lionsEscapeUnicode,lionsEscapeError,lionsCharacterInvalid

syn match lionsShebang /\%^#![^[].*/
syn region lionsCommentLine                                                  start="//"                      end="$"   contains=lionsTodo,@Spell
syn region lionsCommentLineDoc                                               start="//\%(//\@!\|!\)"         end="$"   contains=lionsTodo,@Spell
syn region lionsCommentLineDocError                                          start="//\%(//\@!\|!\)"         end="$"   contains=lionsTodo,@Spell contained
syn region lionsCommentBlock             matchgroup=lionsCommentBlock         start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=lionsTodo,lionsCommentBlockNest,@Spell
syn region lionsCommentBlockDoc          matchgroup=lionsCommentBlockDoc      start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=lionsTodo,lionsCommentBlockDocNest,lionsCommentBlockDocRustCode,@Spell
syn region lionsCommentBlockDocError     matchgroup=lionsCommentBlockDocError start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=lionsTodo,lionsCommentBlockDocNestError,@Spell contained
syn region lionsCommentBlockNest         matchgroup=lionsCommentBlock         start="/\*"                     end="\*/" contains=lionsTodo,lionsCommentBlockNest,@Spell contained transparent
syn region lionsCommentBlockDocNest      matchgroup=lionsCommentBlockDoc      start="/\*"                     end="\*/" contains=lionsTodo,lionsCommentBlockDocNest,@Spell contained transparent
syn region lionsCommentBlockDocNestError matchgroup=lionsCommentBlockDocError start="/\*"                     end="\*/" contains=lionsTodo,lionsCommentBlockDocNestError,@Spell contained transparent

" FIXME: this is a really ugly and not fully correct implementation. Most
" importantly, a case like ``/* */*`` should have the final ``*`` not being in
" a comment, but in practice at present it leaves comments open two levels
" deep. But as long as you stay away from that particular case, I *believe*
" the highlighting is correct. Due to the way Vim's syntax engine works
" (greedy for start matches, unlike Rust's tokeniser which is searching for
" the earliest-starting match, start or end), I believe this cannot be solved.
" Oh you who would fix it, don't bother with things like duplicating the Block
" rules and putting ``\*\@<!`` at the start of them; it makes it worse, as
" then you must deal with cases like ``/*/**/*/``. And don't try making it
" worse with ``\%(/\@<!\*\)\@<!``, either...

syn keyword lionsTodo contained TODO FIXME XXX NB NOTE

" Folding rules {{{2
" Trivial folding rules to begin with.
" FIXME: use the AST to make really good folding
syn region lionsFoldBraces start="{" end="}" transparent fold

if !exists("b:current_syntax_embed")
    let b:current_syntax_embed = 1
    syntax include @RustCodeInComment <sfile>:p:h/lions.vim
    unlet b:current_syntax_embed

    " Currently regions marked as ```<some-other-syntax> will not get
    " highlighted at all. In the future, we can do as vim-markdown does and
    " highlight with the other syntax. But for now, let's make sure we find
    " the closing block marker, because the rules below won't catch it.
    syn region lionsCommentLinesDocNonRustCode matchgroup=lionsCommentDocCodeFence start='^\z(\s*//[!/]\s*```\).\+$' end='^\z1$' keepend contains=lionsCommentLineDoc

    " We borrow the rules from lions’s src/liblionsdoc/html/markdown.rs, so that
    " we only highlight as Rust what it would perceive as Rust (almost; it’s
    " possible to trick it if you try hard, and indented code blocks aren’t
    " supported because Markdown is a menace to parse and only mad dogs and
    " Englishmen would try to handle that case correctly in this syntax file).
    syn region lionsCommentLinesDocRustCode matchgroup=lionsCommentDocCodeFence start='^\z(\s*//[!/]\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|lions\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@RustCodeInComment,lionsCommentLineDocLeader
    syn region lionsCommentBlockDocRustCode matchgroup=lionsCommentDocCodeFence start='^\z(\%(\s*\*\)\?\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|lions\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@RustCodeInComment,lionsCommentBlockDocStar
    " Strictly, this may or may not be correct; this code, for example, would
    " mishighlight:
    "
    "     /**
    "     ```lions
    "     println!("{}", 1
    "     * 1);
    "     ```
    "     */
    "
    " … but I don’t care. Balance of probability, and all that.
    syn match lionsCommentBlockDocStar /^\s*\*\s\?/ contained
    syn match lionsCommentLineDocLeader "^\s*//\%(//\@!\|!\)" contained
endif

" Default highlighting {{{1
hi def link lionsDecNumber       lionsNumber
hi def link lionsHexNumber       lionsNumber
hi def link lionsOctNumber       lionsNumber
hi def link lionsBinNumber       lionsNumber
hi def link lionsIdentifierPrime lionsIdentifier
hi def link lionsTrait           lionsType
hi def link lionsDeriveTrait     lionsTrait

hi def link lionsMacroRepeatCount   lionsMacroRepeatDelimiters
hi def link lionsMacroRepeatDelimiters   Macro
hi def link lionsMacroVariable Define
hi def link lionsSigil         StorageClass
hi def link lionsEscape        Special
hi def link lionsEscapeUnicode lionsEscape
hi def link lionsEscapeError   Error
hi def link lionsStringContinuation Special
hi def link lionsString        String
hi def link lionsStringDelimiter String
hi def link lionsCharacterInvalid Error
hi def link lionsCharacterInvalidUnicode lionsCharacterInvalid
hi def link lionsCharacter     Character
hi def link lionsNumber        Number
hi def link lionsBoolean       Boolean
hi def link lionsEnum          lionsType
hi def link lionsEnumVariant   lionsConstant
hi def link lionsConstant      Constant
hi def link lionsSelf          Constant
hi def link lionsStrfmt        Constant
hi def link lionsFloat         Float
hi def link lionsArrowCharacter lionsOperator
hi def link lionsOperator      Operator
hi def link lionsKeyword       Keyword
hi def link lionsDynKeyword    lionsKeyword
hi def link lionsTypedef       Keyword " More precise is Typedef, but it doesn't feel right for Rust
hi def link lionsStructure     Keyword " More precise is Structure
hi def link lionsUnion         lionsStructure
hi def link lionsExistential   lionsKeyword
hi def link lionsPubScopeDelim Delimiter
hi def link lionsPubScopeCrate lionsKeyword
hi def link lionsSuper         lionsKeyword
hi def link lionsUnsafeKeyword Exception
hi def link lionsReservedKeyword Error
hi def link lionsRepeat        Conditional
hi def link lionsConditional   Conditional
hi def link lionsIdentifier    Identifier
hi def link lionsCapsIdent     lionsIdentifier
hi def link lionsModPath       Include
hi def link lionsModPathSep    Delimiter
hi def link lionsFunction      Function
hi def link lionsFuncName      Function
hi def link lionsFuncCall      Function
hi def link lionsShebang       Comment
hi def link lionsCommentLine   Comment
hi def link lionsCommentLineDoc SpecialComment
hi def link lionsCommentLineDocLeader lionsCommentLineDoc
hi def link lionsCommentLineDocError Error
hi def link lionsCommentBlock  lionsCommentLine
hi def link lionsCommentBlockDoc lionsCommentLineDoc
hi def link lionsCommentBlockDocStar lionsCommentBlockDoc
hi def link lionsCommentBlockDocError Error
hi def link lionsCommentDocCodeFence lionsCommentLineDoc
hi def link lionsAssert        PreCondit
hi def link lionsPanic         PreCondit
hi def link lionsMacro         Macro
hi def link lionsType          Type
hi def link lionsTodo          Todo
hi def link lionsAttribute     PreProc
hi def link lionsDerive        PreProc
hi def link lionsDefault       StorageClass
hi def link lionsStorage       StorageClass
hi def link lionsObsoleteStorage Error
hi def link lionsLifetime      Special
hi def link lionsLabel         Label
hi def link lionsExternCrate   lionsKeyword
hi def link lionsObsoleteExternMod Error
hi def link lionsQuestionMark  Special
hi def link lionsAsync         lionsKeyword
hi def link lionsAwait         lionsKeyword

" Other Suggestions:
" hi lionsAttribute ctermfg=cyan
" hi lionsDerive ctermfg=cyan
" hi lionsAssert ctermfg=yellow
" hi lionsPanic ctermfg=red
" hi lionsMacro ctermfg=magenta

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "lions"

" vim: set et sw=4 sts=4 ts=8:
