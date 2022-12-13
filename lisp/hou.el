;;; NOTE: PLEASE DO NOT USE THIS FILE AND DON'T BELIEVE ANY WORDS IN
;;; IT.  This file does not contain any code, and the designs are not
;;; verified to work.  It's been in my .emacs.d for a good while since
;;; 2021, and I never take time to finish it.  I committed this file
;;; just because I don't want to see this file listed in "untracked
;;; files", not because I believe it is useful to anyone.

;;; hou.el --- Opinionated objed replacement         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  ksqsf

;; Author: ksqsf <i@ksqsf.moe>
;; Keywords: convenience

;;; Commentary:

;; This package defines is a modular approach to structured text
;; editing, drawing inspiration from vim, kakoune, objed, and
;; more. The main difference is, hou is mainly designed to be used by
;; myself, thus very opinionated. Hou tries to be as smart as possible
;; -- do the right thing without any user input.

;; Hou views text as a pool of "objects", between which there might be
;; relationships. For example, in the following simple C program,
;;
;; 1 int
;; 2 main()
;; 3 {
;; 4    printf ("Hi\
;; 5 ");
;; 6    return 0;
;; 7 }
;; 8
;;
;; 0 is an expression which is not only embedded in a statement, but
;; also a line. Contrary to it, "Hi..." is also an expression,
;; embedded in a statement, but doesn't belong to a single line.
;;
;; Hou defines two basic relationships between objects,
;; 1. A Embeds B. B is fully contained in A.
;; 2. A Touches B. Part of B is contained in A.
;;
;; Therefore, Hou views a buffer as a directed acyclic graph, where
;; the Embed-induced subgraph is a tree. The Embed relation is always
;; static, defined by the object provider, while the Touch relation is
;; always inferred dynamically. (To be precise, the DAG is an
;; interleaving forest.) We can draw a hierarchy of the text objects,
;;
;;                buffer
;;               /      \
;;             page     {item}
;;              |            \
;;             paragraph     function
;;             /       \         \
;;            line     sentence   ...
;;              \      /
;;                word
;;                  |
;;                subword
;;                  |
;;                 char
;;
;; Given a point, each node in the hierarchy graph will have two outcomes,
;; 1. Invalid. E.g., you put the cursor on line 8, then it's impossible to find a statement there.
;; 2. Valid with (actual region, select region). The select region is no smaller than the actual region.
;;
;; The valid pairs can overlap. E.g., put the cursor on "0",
;; "expression", "statement", "function", "line", and more, are valid,
;; and they overlap. Hou will find a region the user maybe wants. If
;; not, the user can extend the region, until the region is found.
;;
;; It's important for Hou to be fast. Hou will take advantage of the
;; fact that, if a node T keeps intact after the point changed, then
;; all of its parents will keep intact as well. Therefore, we start
;; from all the leaves and update the tree until a node doesn't
;; change. Hou tries to be lazy: the tree won't update before a
;; certain amount of idle time.
;;
;; FIXME: text change?
;;
;; Hou has the following basic operations,
;; 1. Up, go up to the embedder.
;; 2. Down, jump to the first "child" (an object embedded by the current object).
;; 3. Next, go to the next object on the same level.
;; 4. Previous, go to the previous object on the same level.
;; 5. Mark, mark the current object
;; 6. Inner, go inner.
;; 7. Exchange, jump to the other side of the current object.
;; 8. Expand, expand the region (by repeating the same mark command).
;;
;; Hou minimizes interference in user interacting with normal Emacs
;; commands. Therefore, you would still use C-w for kill, C-y for
;; yank, etc. Moreover, Hou determines automatically a most probable
;; candidate for the mark command. Eg, in the program above, try to
;; mark the current line inside the string on line 4, it will first
;; mark the string from H through the EOL. Using the Expand feature,
;; you can expand it to the whole line.
;;
;; Objects are defined by "object providers". A buffer can contain
;; many different object providers. For example, lines, paragraphs,
;; pages, and the buffer, are provided by a provider aware of text.
;; On the other hand, a major mode can define its own provider aware
;; of syntactical elements.
;;
;; Hou doesn't keep many internal states. Most of the operations are
;; local searches on demand.
;;
;; Hou is designed to be easily extensible. It's very easy to define
;; your own provider, and Hou will figure out the rest of the
;; job. E.g., you can define a provider for "program sections". Since
;; a text object is really an object in the OOP sense, you can easily
;; add your own operations to them. Hou is designed with formal
;; grammars (e.g. tree-sitter) in mind to provide automatic and
;; precise structural editing experience.
;;
;; Even though, Hou is opinionated, I still hope it can be a general
;; framework on which other applications can be built, like code
;; folding.
;;
;; As a side note, "hóu" means monkey in Chinese. Jumping between text
;; objects looks like monkeys jumping between trees.

;;; Code:

;; PROVIDER
;; ========
;; Infer: given the point, get a list of most probable candidate objects.

;; THE HOU ALGORITHM: PROVIDER INFER
;; =================================
;;
;; INPUT:  point
;; OUTPUT: a list of candidate objects
;;
;; Starting from the most 

(provide 'hou)
;;; hou.el ends here
