;;; cdmi-mode.el --- an emacs mode for working with CDMI data. -*- coding: utf-8-unix -*-

;; Copyright (C) 2012 John Eastman

;; Version  : 0.0.1
;; Keywords : cdmi major mode
;; Author   : John Eastman <john.eastman@gmail.org>
;; URL: http://github.com/jeastman/cdmi-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides syntax highlighting

;;; For more information
;; CDMI Specification: http://cdmi.snia.org

;;; Acknowledgements
;; cdmi-send-request based on code from fluidinfo.el
;; Copyright (C) 2009, 2010 Holger Durer
;; https://github.com/hdurer/fluiddb.el/blob/master/fluidinfo.el

;;; Code:

(require 'easymenu)

;;; Constants

(defconst cdmi-mode-version "0.0.1"
  "The version of `cdmi-mode'.")

;;; Customizable Variables

(defgroup cdmi nil
  "Interacting with CDMI in Emacs."
  :prefix "cdmi-"
  :group 'tools)

(defcustom cdmi-beautify-command "python -mjson.tool"
  "The command to run for beautification of json values."
  :group 'cdmi
  :type 'string)

(defcustom cdmi-spec-version "1.0.1"
  "The CDMI client specification version to use."
  :group 'cdmi
  :type 'string)

(defcustom cdmi-server nil
  "The CDMI server to interact with."
  :group 'cdmi
  :type 'string)

(defcustom cdmi-prefix "cdmi/"
  "Prefix to use when accessing CDMI. Include trailing slash."
  :group 'cdmi
  :type 'string)

(defcustom cdmi-secure t
  "Connect to server securely (HTTPS)."
  :group 'cdmi
  :type 'boolean)

;; Allow others to hook in their own code
(defcustom cdmi-mode-hook nil
  "Hook called by `cdmi-mode'."
  :type 'hook
  :group 'cdmi)

(defvar cdmi-buffer-name "CDMI")

(defvar cdmi-resource-history nil)

(defvar cdmi-current-resource nil
  "The path to the current resource.")
(make-variable-buffer-local 'cdmi-current-resource)
(put 'cdmi-current-resource 'permanent-local t)

;; Define the keymap
(defvar cdmi-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for CDMI major mode.")

;;; Commands

(defun cdmi-mode-customize ()
  "Run \\[customize-group] for the `cdmi' group."
  (interactive)
  (customize-group 'cdmi))

(defun cdmi-version ()
  "Show the `cdmi-mode' version in the echo area."
  (interactive)
  (message (concat "cdmi-mode, version " cdmi-mode-version)))

;;; Menubar

(easy-menu-define cdmi-mode-menu cdmi-mode-map
  "Menu for CDMI mode"
  '("CDMI"
    ["Version" cdmi-version]
    ))

;;; Utilities

(defun cdmi-beautify ()
  "Beautify javascript"
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           cdmi-beautify-command nil t))

;;; Communications
(defun cdmi-send-request (method url-extra query-args body accept-value extra-headers)
  "General purpose helper function to do the actual CDMI call."
  (let ((extra-headers extra-headers))
    (save-excursion
      (when query-args
        (loop
         for first = t then nil
         for (param . value) in query-args
         do (setq url-extra
                  (concat url-extra
                          (if first "?" "&")
                          param
                          "="
                          value))))
      (let* ((*cdmi-within-call* t)
             (url-request-method method)
             (url-http-attempt-keepalives nil)
             (url-mime-accept-string accept-value)
             (url-mime-charset-string nil)
             (url-extensions-header nil)
             (url-request-data body)
             (url-request-extra-headers (cons
                                         (cons "X-CDMI-Specification-Version" cdmi-spec-version)
                                         extra-headers))
             (url (concat
                   (if cdmi-secure "https" "http")
                   "://" cdmi-server "/" cdmi-prefix url-extra))
             (buffer (url-retrieve-synchronously url))
             result)
        (switch-to-buffer buffer)
        (setq result (url-http-parse-headers))
        (goto-char (if (re-search-forward "^\r?$" nil 1)
                       (match-beginning 0)
                     (point-max)))
        (move-end-of-line nil)
        (forward-char)
        (let* ((status url-http-response-status)
               (status-ok (and (<= 200 status)
                               (<= status 299)))
               (content-type url-http-content-type)
               ;; hack to remove errors returned from transport
               ;; still looking into why these are returned
               ;; seems to only occur with secure connections
               (content (replace-regexp-in-string "\\*\\*\\*.*$" "" (buffer-substring (point) (point-max))))
               (error-class (unless status-ok "ERROR")))
          (kill-buffer buffer)
          (if status-ok
              (list status-ok content status content-type)
            (list status-ok content status content-type error-class)))))))

(defun cdmi-get (path ct)
  "Retrieve the content of a CDMI path."
  (cdmi-send-request "GET"
                     path
                     nil
                     nil
                     ct
                     nil))

(defun cdmi-clear-buffer ()
  "Clear the contents of the CDMI buffer."
  (interactive)
  (switch-to-buffer
   (get-buffer-create cdmi-buffer-name))
  (goto-char (point-min))
  (clipboard-kill-region 1 (point-max))
  (beginning-of-buffer))

(defun cdmi-open-in-buffer (path accept-value)
  "Open content of a resource in the CDMI buffer."
  (switch-to-buffer
   (get-buffer-create cdmi-buffer-name))
  (cdmi-clear-buffer)
  (insert (cadr (cdmi-get path accept-value)))
  (cdmi-beautify)
  (cdmi-mode)
  (set-buffer-modified-p nil)
  (setq cdmi-current-resource path)
  path)

(defun cdmi-open-container (path)
  "Open the content of a CDMI container."
  (interactive
   (list (read-string "path: "
                      (if cdmi-current-resource cdmi-current-resource "")
                      'cdmi-resource-history)))
  (cdmi-open-in-buffer path "application/cdmi-container"))

(defun cdmi-open-object (path)
  "Open the content of a CDMI object."
  (interactive
   (list (read-string "path: "
                      (if cdmi-current-resource cdmi-current-resource "")
                      'cdmi-resource-history))) 
  (cdmi-open-in-buffer path "application/cdmi-object"))

(defun cdmi-open-domain (path)
  "Open the content of a CDMI domain."
  (interactive
   (list (read-string "path: "
                      (if cdmi-current-resource cdmi-current-resource "")
                      'cdmi-resource-history)))
  (cdmi-open-in-buffer path "application/cdmi-domain"))

(defun cdmi-open-queue (path)
  "Open the content of a CDMI queue."
  (interactive
   (list (read-string "path: "
                      (if cdmi-current-resource cdmi-current-resource "")
                      'cdmi-resource-history)))
  (cdmi-open-in-buffer path "application/cdmi-queue"))

(defun cdmi-open-capability (path)
  "Open the content of a CDMI capability."
  (interactive
   (list (read-string "path: " (if cdmi-current-resource cdmi-current-resource "")
                      'cdmi-resource-history)))
  (cdmi-open-in-buffer path "application/cdmi-capability"))

(defun cdmi-open (path accept-value)
  "Open a CDMI resource specified by `path' in the CDMI buffer."
  (interactive
   (list
    (read-string "path: "
                 (if cdmi-current-resource cdmi-current-resource "")
                 'cdmi-resource-history)
         (read-string "accept: " "*/*" nil
                      (list "*/*" "application/cdmi-object"
                       "application/cdmi-container" "application/cdmi-domain"
                       "application/cdmi-queue" "application/cdmi-capability"))))
  (cdmi-open-in-buffer path accept-value))

;; define classes of keywords
(defvar cdmi-keywords
  '("capabilitiesURI" "children" "childrenrange" "completionStatus" "copy" "deserialize" "deserializevalue" "domainURI" "exports" "mimetype" "metadata" "move" "parentID" "parentURI" "percentComplete" "objectID" "objectName" "objectType" "queueValues" "reference" "serialize" "snapshots" "value" "valuerange" "valuetransferencoding")
  "CDMI keywords.")
(defvar cdmi-transferencodings
  '("utf-8" "base64" "json")
  "CDMI transfer encodings.")
(defvar cdmi-booleans
  '("true" "false")
  "CDMI booleans.")
(defvar cdmi-types
  '("aceflags" "acemask" "acetype" "cdmi_acl" "cdmi_acount" "cdmi_assignedsize" "cdmi_atime" "cdmi_capabilities" "cdmi_ctime" "cdmi_dataobjects" "cdmi_data_autodelete" "cdmi_data_dispersion" "cdmi_data_dispersion_provided" "cdmi_data_holds" "cdmi_data_redundancy" "cdmi_data_redundancy_provided" "cdmi_data_retention" "cdmi_delegation_URI" "cdmi_domains" "cdmi_domain_enabled" "cdmi_domain_delete_reassign" "cdmi_domainURI" "cdmi_encryption" "cdmi_encryption_provided" "cdmi_export_cifs" "cdmi_export_iscsi" "cdmi_export_nfs" "cdmi_export_occi_iscsi" "cdmi_export_webdav" "cdmi_geographic_placement" "cdmi_geographic_placement_provided" "cdmi_hash" "cdmi_hold_id" "cdmi_hold_id_provided" "cdmi_immediate_redundancy" "cdmi_immediate_redundancy_provided" "cdmi_infrastructure_redundancy" "cdmi_infrastructure_redundancy_provided" "cdmi_latency" "cdmi_latency_provided" "cdmi_logging" "cdmi_logging_class" "cdmi_logging_status" "cdmi_member_credentials" "cdmi_member_enabled" "cdmi_member_groups" "cdmi_member_name" "cdmi_member_principal" "cdmi_member_privileges" "cdmi_member_type" "cdmi_metadata_maxitems" "cdmi_metadata_maxsize" "cdmi_metadata_maxtotalsize" "cdmi_mcount" "cdmi_mtime" "cdmi_notification" "cdmi_notification_events" "cdmi_objectid" "cdmi_object_access_by_ID" "cdmi_owner" "cdmi_query" "cdmi_query_contains" "cdmi_query_regex" "cdmi_query_status" "cdmi_query_tags" "cdmi_query_value" "cdmi_queues" "cdmi_queue_type" "cdmi_references" "cdmi_results_specification" "cdmi_retention_autodelete" "cdmi_retention_autodelete_provided" "cdmi_retention_id" "cdmi_retention_period" "cdmi_retention_period_provided" "cdmi_RPO" "cdmi_RPO_provided" "cdmi_RTO" "cdmi_RTO_provided" "cdmi_sanitization_method" "cdmi_sanitization_method_provided" "cdmi_scope_specification" "cdmi_security_audit" "cdmi_security_access_control" "cdmi_security_data_integrity" "cdmi_security_encryption" "cdmi_security_immutability" "cdmi_security_sanitization" "cdmi_serialization_json" "cdmi_size" "cdmi_snapshots" "cdmi_summary_bytehours" "cdmi_summary_bytesaverage" "cdmi_summary_bytesmax" "cdmi_summary_bytesmin" "cdmi_summary_charge" "cdmi_summary_end" "cdmi_summary_gets" "cdmi_summary_kwaverage" "cdmi_summary_kwhours" "cdmi_summary_kwmax" "cdmi_summary_kwmin" "cdmi_summary_objectaverage" "cdmi_summary_objecthours" "cdmi_summary_objectmax" "cdmi_summary_objectmin" "cdmi_summary_puts" "cdmi_summary_reads" "cdmi_summary_start" "cdmi_summary_writes" "cdmi_throughput" "cdmi_throughput_provided" "cdmi_value_hash" "cdmi_value_hash_provided" "cdmi_versions" "cdmi_versions_count_provided" "identifier")
  "CDMI types.")
(defvar cdmi-constants
  '("ADD_OBJECT" "ADD_SUBCONTAINER" "ADMINISTRATOR@" "ADMINUSERS@" "ANONYMOUS@" "APPEND_DATA" "AUTHENTICATED@" "CDMI_ACE_ACCESS_ALLOW" "CDMI_ACE_ACCESS_DENY" "CDMI_ACE_ADD_OBJECT" "CDMI_ACE_ADD_SUBCONTAINER" "CDMI_ACE_APPEND_DATA" "CDMI_ACE_DELETE" "CDMI_ACE_DELETE_OBJECT" "CDMI_ACE_DELETE_SUBCONTAINER" "CDMI_ACE_EXECUTE" "CDMI_ACE_FLAGS_CONTAINER_INHERIT_ACE" "CDMI_ACE_FLAGS_IDENTIFIER_GROUP" "CDMI_ACE_FLAGS_INHERITED_ACE" "CDMI_ACE_FLAGS_NONE" "CDMI_ACE_FLAGS_NO_PROPAGATE_ACE" "CDMI_ACE_FLAGS_OBJECT_INHERIT_ACE" "CDMI_ACE_FLAGS_INHERIT_ONLY_ACE" "CDMI_ACE_LIST_CONTAINER" "CDMI_ACE_READ_ACL" "CDMI_ACE_READ_ATTRIBUTES" "CDMI_ACE_READ_METADATA" "CDMI_ACE_READ_OBJECT" "CDMI_ACE_SYNCHRONIZE" "CDMI_ACE_SYSTEM_AUDIT" "CDMI_ACE_WRITE_ACL" "CDMI_ACE_WRITE_ATTRIBUTES" "CDMI_ACE_WRITE_METADATA" "CDMI_ACE_WRITE_OBJECT" "CDMI_ACE_WRITE_OWNER" "CDMI_ACE_WRITE_RETENTION" "CDMI_ACE_WRITE_RETENTION_HOLD" "CONTAINER_INHERIT" "DELETE" "DELETE_OBJECT" "DELETE_SUBCONTAINER" "EVERYONE@" "EXECUTE" "GROUP@" "IDENTIFIER_GROUP" "INHERITED" "INHERIT_ONLY" "LIST_CONTAINER" "NO_FLAGS" "NO_PROPAGATE" "OBJECT_INHERIT" "OWNER@" "READ_ACL" "READ_ATTRIBUTES" "READ_METADATA" "READ_OBJECT" "SYNCHRONIZE" "WRITE_ACL" "WRITE_ATTRIBUTES" "WRITE_METADATA" "WRITE_OBJECT" "WRITE_OWNER" "WRITE_RETENTION" "WRITE_RETENTION_HOLD")
  "CDMI constants.")
(defvar cdmi-events
  '("Complete" "Processing" "Error")
  "CDMI events.")
(defvar cdmi-functions
  '("cdmi_copy_dataobject_by_ID" "cdmi_copy_container" "cdmi_copy_dataobject" "cdmi_copy_domain" "cdmi_copy_queue" "cdmi_copy_queue_by_ID" "cdmi_create_container" "cdmi_create_dataobject" "cdmi_create_domain" "cdmi_create_queue" "cdmi_create_reference" "cdmi_create_reference_by_ID" "cdmi_delete_container" "cdmi_delete_dataobject" "cdmi_delete_domain" "cdmi_delete_queue" "cdmi_deserialize_container" "cdmi_deserialize_dataobject" "cdmi_deserialize_dataobject_by_ID" "cdmi_deserialize_domain" "cdmi_deserialize_queue" "cdmi_deserialize_queue_by_ID" "cdmi_domain_members" "cdmi_domain_summary" "cdmi_export_container_cifs" "cdmi_export_container_iscsi" "cdmi_export_container_nfs" "cdmi_export_container_occi" "cdmi_export_container_webdav" "cdmi_list_children" "cdmi_list_children_range" "cdmi_modify_deserialize_dataobject" "cdmi_modify_deserialize_container" "cdmi_modify_deserialize_domain" "cdmi_modify_deserialize_queue" "cdmi_modify_metadata" "cdmi_modify_value" "cdmi_modify_value_range" "cdmi_move_container" "cdmi_move_dataobject" "cdmi_move_queue" "cdmi_object_move_from_ID" "cdmi_object_copy_from_local" "cdmi_object_copy_from_remote" "cdmi_object_move_from_local" "cdmi_object_move_from_remote" "cdmi_object_move_to_ID" "cdmi_post_dataobject" "cdmi_post_dataobject_by_ID" "cdmi_post_queue" "cdmi_post_queue_by_ID" "cdmi_read_metadata" "cdmi_read_value" "cdmi_read_value_range" "cdmi_reference_queue" "cdmi_serialize_domain" "cdmi_serialize_container" "cdmi_serialize_container_to_ID" "cdmi_serialize_dataobject" "cdmi_serialize_dataobject_to_ID" "cdmi_serialize_domain_to_ID" "cdmi_serialize_queue" "cdmi_serialize_queue_to_ID" "cdmi_snapshot")
  "CDMI functions.")

;; create regex string for each class of keywords
(defvar cdmi-keyword-regexp (regexp-opt cdmi-keywords 'words))
(defvar cdmi-transferencoding-regexp (regexp-opt cdmi-transferencodings 'words))
(defvar cdmi-boolean-regexp (regexp-opt cdmi-booleans 'words))
(defvar cdmi-type-regexp (regexp-opt cdmi-types 'words))
(defvar cdmi-constant-regexp (regexp-opt cdmi-constants 'words))
(defvar cdmi-event-regexp (regexp-opt cdmi-events 'words))
(defvar cdmi-function-regexp (regexp-opt cdmi-functions 'words))

;; clear memory
(setq cdmi-keywords nil)
(setq cdmi-transferencodings nil)
(setq cdmi-booleans nil)
(setq cdmi-types nil)
(setq cdmi-constants nil)
(setq cdmi-events nil)
(setq cdmi-functions nil)

;; create the list for font-lock
;; give each class of keyword its own face
(setq cdmi-font-lock-keywords
      `(
        (,cdmi-type-regexp . font-lock-type-face)
        (,cdmi-constant-regexp . font-lock-constant-face)
        (,cdmi-event-regexp . font-lock-builtin-face)
        (,cdmi-function-regexp . font-lock-function-name-face)
        (,cdmi-boolean-regexp . font-lock-constant-face)
        (,cdmi-transferencoding-regexp . font-lock-keyword-face)
        (,cdmi-keyword-regexp . font-lock-keyword-face)
        ;; note: order above matters
        ))

;;;###autoload
(define-derived-mode cdmi-mode fundamental-mode
  "cdmi"
  "Major mode for editing CDMI"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((cdmi-font-lock-keywords) t))

  ;; clear memory
  (setq cdmi-keywords-regexp nil)
  (setq cdmi-transferencoding-regexp nil)
  (setq cdmi-boolean-regexp nil)
  (setq cdmi-type-regexp nil)
  (setq cdmi-constant-regexp nil)
  (setq cdmi-event-regexp nil)
  (setq cdmi-function-regexp nil)
)

(provide 'cdmi-mode)

;;; cdmi-mode.el ends here
