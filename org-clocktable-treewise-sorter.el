;;; org-clocktable-treewise-sorter.el --- Utilities for sorting clocktable entries with respect to levels
;;
;; Copyright (C) 2019 Lei Tan <louietanlei@gmail.com>
;; 
;; Description: 
;; Author: Lei Tan
;; Created: Sun Apr  7 18:20:04 2019 (+0800)
;; Version: 0.1
;; Package-Requires: ((emacs "25"))
;; Last-Updated: Sun Apr  7 18:20:04 2019 (+0800)
;; URL: https://github.com/louietan/org-clocktable-treewise-sorter
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  When sorting a clocktable with the builtin formatter
;;  `org-clocktable-write-default', entries are simply sorted row by
;;  row ignoring levels and would be out of order should the levels of
;;  hierarchy be more than one.  This file provides utilities to sort
;;  clocktables with respect to entry levels.  The way it works is to
;;  sort the table data before being written into a Org table, thus
;;  has its limitations that it has no idea of "columns" and can only
;;  sort on name and time.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defconst lt/org-clocktable--comparison-fns
  '((name . string<)
    (NAME . string>)
    (time . <)
    (TIME . >)))

(defconst lt/org-clocktable--file-component-fns
  '((name . (lambda (a) (file-name-base (car a))))
    (NAME . (lambda (a) (file-name-base (car a))))
    (time . cadr)
    (TIME . cadr)))

(defconst lt/org-clocktable--entry-component-fns
  '((name . (lambda (a) (org-link-unescape (cadr a))))
    (NAME . (lambda (a) (org-link-unescape (cadr a))))
    (time . cadddr)
    (TIME . cadddr)))

(defun lt/org-clocktable-make-tree (entries)
  "Build a tree from ENTRIES according to their levels and order."
  ;; stack is list of (entry . children)
  ;; entry with level 0 is the root of all entries
  (let ((stack (list (cons (list 0) nil)))
        last-level cur-level)
    (dolist (entry entries)
      (unless last-level (setq last-level (car entry)))
      (setq cur-level (car entry))
      (cond
       ;; subtree begins
       ((< last-level cur-level)
        ;; push last entry to stack to make it the last tree
        (push (cadar stack) stack))
       ;; subtree ends
       ((> last-level cur-level)
        ;; clean the stack to end the last subtree
        (while (>= (caaar stack) cur-level)
          (let ((top (car stack))
                (rst (cdr stack)))
            (setq stack rst)))))
      ;; append entry to the subtree at top
      (push (cons entry nil) (cdar stack))
      (setq last-level cur-level))
    ;; clear stack
    (while (/= (caaar stack) 0)
      (setq stack (cdr stack)))
    (car stack)))

(defun lt/org-clocktable-sort-tree (tree &optional order)
  "Sort TREE recursively in ORDER."
  (when-let ((children (cdr tree))
             (compare-entries (alist-get order lt/org-clocktable--comparison-fns))
             (entry-component (alist-get order lt/org-clocktable--entry-component-fns)))
    (setq children
          (setcdr tree
                  (sort children
                        (lambda (a b)
                          (funcall compare-entries
                                   (funcall entry-component (car a))
                                   (funcall entry-component (car b)))))))
    (dolist (child children tree)
      (lt/org-clocktable-sort-tree child order))))

(defun lt/org-clocktable-flatten-tree (tree)
  "Get a list of entries back from TREE."
  (let ((entries (list (car tree)))
        (children (cdr tree)))
    (dolist (child children entries)
      (setq entries (append entries (lt/org-clocktable-flatten-tree child))))))

(defun lt/org-clocktable-sort-entries-treewise (entries &optional order)
  "Sort ENTRIES tree-wise in ORDER."
  (cdr
   (lt/org-clocktable-flatten-tree
    (lt/org-clocktable-sort-tree
     (lt/org-clocktable-make-tree entries)
     order))))

(defun lt/org-clocktable-sort-tables-treewise (tables &optional order)
  "Sort TABLES tree-wise in ORDER."
  (dolist (tbl tables)
    (setf (nth 2 tbl)
          (lt/org-clocktable-sort-entries-treewise (nth 2 tbl) order)))
  (let ((file-component (alist-get order lt/org-clocktable--file-component-fns))
        (compare-files (alist-get order lt/org-clocktable--comparison-fns)))
    (sort tables
          (lambda (a b)
            (funcall compare-files
                     (funcall file-component a)
                     (funcall file-component b))))))

(defun lt/org-clocktable-write-sorted-trees (ipos tables params)
  "Clocktable formatter that sorts TABLES with respect to entry levels and delegates the actual formatting to `org-clock-clocktable-formatter' or `org-clocktable-write-default'."
  (when-let ((sort (plist-get params :sort-trees))
             ((member sort '(time TIME name NAME))))
    (setq tables (lt/org-clocktable-sort-tables-treewise tables sort)))
  (funcall (if (eq 'lt/org-clocktable-write-sorted-trees
                   org-clock-clocktable-formatter)
               'org-clocktable-write-default
             org-clock-clocktable-formatter)
           ipos tables params))

(provide 'org-clocktable-treewise-sorter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-clocktable-treewise-sorter.el ends here
