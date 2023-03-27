;;; sql-oracle-fontlock.el --- fontlock for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2007, 2017 Dr. Volker Zell <vzell@volkerzell.de>

;; Author: Dr. Volker Zell
;; Maintainer: Dr. Volker Zell
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 2.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle, fontlock

;; This file is not part of GNU Emacs yet.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See the documentation in sql-oracle-mode.el

;;; ChangeLog:

;; 01.11.2017 vzell: Added new SQL JOIN syntax keywords 

;; 

;;; Code:

(require 'regexp-opt)

(defvar sql-oracle-ddview-alist '(("all_all_tables") ("all_apply") ("all_apply_conflict_columns") ("all_apply_dml_handlers") ("all_apply_enqueue") ("all_apply_error") ("all_apply_execute") ("all_apply_key_columns") ("all_apply_parameters") ("all_apply_progress") ("all_apply_table_columns") ("all_arguments") ("all_associations") ("all_audit_policies") ("all_audit_policy_columns") ("all_aw_ps") ("all_aws") ("all_base_table_mviews") ("all_capture") ("all_capture_extra_attributes") ("all_capture_parameters") ("all_capture_prepared_database") ("all_capture_prepared_schemas") ("all_capture_prepared_tables") ("all_catalog") ("all_cluster_hash_expressions") ("all_clusters") ("all_col_comments") ("all_col_privs") ("all_col_privs_made") ("all_col_privs_recd") ("all_coll_types") ("all_cons_columns") ("all_cons_obj_columns") ("all_constraints") ("all_context") ("all_db_links") ("all_def_audit_opts") ("all_dependencies") ("all_dim_attributes") ("all_dim_child_of") ("all_dim_hierarchies") ("all_dim_join_key") ("all_dim_level_key") ("all_dim_levels") ("all_dimensions") ("all_directories") ("all_errors") ("all_evaluation_context_tables") ("all_evaluation_context_vars") ("all_evaluation_contexts") ("all_external_locations") ("all_external_tables") ("all_ind_columns") ("all_ind_expressions") ("all_ind_partitions") ("all_ind_statistics") ("all_ind_subpartitions") ("all_indexes") ("all_indextype_arraytypes") ("all_indextype_comments") ("all_indextype_operators") ("all_indextypes") ("all_internal_triggers") ("all_java_arguments") ("all_java_classes") ("all_java_derivations") ("all_java_fields") ("all_java_implements") ("all_java_inners") ("all_java_layouts") ("all_java_methods") ("all_java_ncomps") ("all_java_resolvers") ("all_java_throws") ("all_jobs") ("all_join_ind_columns") ("all_libraries") ("all_lob_partitions") ("all_lob_subpartitions") ("all_lob_templates") ("all_lobs") ("all_log_group_columns") ("all_log_groups") ("all_method_params") ("all_method_results") ("all_mview_aggregates") ("all_mview_analysis") ("all_mview_comments") ("all_mview_detail_relations") ("all_mview_joins") ("all_mview_keys") ("all_mview_logs") ("all_mview_refresh_times") ("all_mviews") ("all_nested_table_cols") ("all_nested_tables") ("all_obj_colattrs") ("all_object_tables") ("all_objects") ("all_opancillary") ("all_oparguments") ("all_opbindings") ("all_operator_comments") ("all_operators") ("all_outline_hints") ("all_outlines") ("all_part_col_statistics") ("all_part_histograms") ("all_part_indexes") ("all_part_key_columns") ("all_part_lobs") ("all_part_tables") ("all_partial_drop_tabs") ("all_pending_conv_tables") ("all_plsql_object_settings") ("all_policies") ("all_policy_contexts") ("all_policy_groups") ("all_procedures") ("all_propagation") ("all_published_columns") ("all_queue_tables") ("all_queues") ("all_refresh") ("all_refresh_children") ("all_refresh_dependencies") ("all_refs") ("all_registered_mviews") ("all_registry_banners") ("all_rewrite_equivalences") ("all_rule_set_rules") ("all_rule_sets") ("all_rules") ("all_scheduler_job_args") ("all_scheduler_job_classes") ("all_scheduler_job_log") ("all_scheduler_job_run_details") ("all_scheduler_jobs") ("all_scheduler_program_args") ("all_scheduler_programs") ("all_scheduler_running_jobs") ("all_scheduler_schedules") ("all_scheduler_window_details") ("all_scheduler_window_groups") ("all_scheduler_window_log") ("all_scheduler_windows") ("all_scheduler_wingroup_members") ("all_sec_relevant_cols") ("all_sequences") ("all_services") ("all_source") ("all_source_tables") ("all_sqlj_type_attrs") ("all_sqlj_type_methods") ("all_sqlj_types") ("all_stored_settings") ("all_streams_global_rules") ("all_streams_message_consumers") ("all_streams_message_rules") ("all_streams_newly_supported") ("all_streams_rules") ("all_streams_schema_rules") ("all_streams_table_rules") ("all_streams_transform_function") ("all_streams_unsupported") ("all_subpart_col_statistics") ("all_subpart_histograms") ("all_subpart_key_columns") ("all_subpartition_templates") ("all_subscribed_columns") ("all_subscribed_tables") ("all_subscriptions") ("all_sumdelta") ("all_synonyms") ("all_tab_col_statistics") ("all_tab_cols") ("all_tab_columns") ("all_tab_comments") ("all_tab_histograms") ("all_tab_modifications") ("all_tab_partitions") ("all_tab_privs") ("all_tab_privs_made") ("all_tab_privs_recd") ("all_tab_statistics") ("all_tab_subpartitions") ("all_tables") ("all_trigger_cols") ("all_triggers") ("all_type_attrs") ("all_type_methods") ("all_type_versions") ("all_types") ("all_unused_col_tabs") ("all_updatable_columns") ("all_users") ("all_ustats") ("all_varrays") ("all_views") ("all_warning_settings") ("all_xml_schemas") ("all_xml_tab_cols") ("all_xml_tables") ("all_xml_view_cols") ("all_xml_views") ("audit_actions") ("catalog") ("cat") ("chained_rows") ("change_sets") ("change_sources") ("change_tables") ("clu") ("col") ("cols") ("database_properties") ("dba_2pc_neighbors") ("dba_2pc_pending") ("dba_advisor_actions") ("dba_advisor_commands") ("dba_advisor_definitions") ("dba_advisor_findings") ("dba_advisor_journal") ("dba_advisor_log") ("dba_advisor_object_types") ("dba_advisor_objects") ("dba_advisor_parameters") ("dba_advisor_rationale") ("dba_advisor_recommendations") ("dba_advisor_sqla_rec_sum") ("dba_advisor_sqla_wk_map") ("dba_advisor_sqla_wk_stmts") ("dba_advisor_sqlw_journal") ("dba_advisor_sqlw_parameters") ("dba_advisor_sqlw_stmts") ("dba_advisor_sqlw_sum") ("dba_advisor_sqlw_tables") ("dba_advisor_sqlw_templates") ("dba_advisor_tasks") ("dba_advisor_templates") ("dba_advisor_usage") ("dba_alert_history") ("dba_all_tables") ("dba_application_roles") ("dba_apply") ("dba_apply_conflict_columns") ("dba_apply_dml_handlers") ("dba_apply_enqueue") ("dba_apply_error") ("dba_apply_execute") ("dba_apply_instantiated_global") ("dba_apply_instantiated_objects") ("dba_apply_instantiated_schemas") ("dba_apply_key_columns") ("dba_apply_parameters") ("dba_apply_progress") ("dba_apply_table_columns") ("dba_aq_agent_privs") ("dba_aq_agents") ("dba_associations") ("dba_attribute_transformations") ("dba_audit_exists") ("dba_audit_object") ("dba_audit_policies") ("dba_audit_policy_columns") ("dba_audit_session") ("dba_audit_statement") ("dba_audit_trail") ("dba_aw_ps") ("dba_aws") ("dba_base_table_mviews") ("dba_blockers") ("dba_capture") ("dba_capture_extra_attributes") ("dba_capture_parameters") ("dba_capture_prepared_database") ("dba_capture_prepared_schemas") ("dba_capture_prepared_tables") ("dba_catalog") ("dba_clu_columns") ("dba_cluster_hash_expressions") ("dba_clusters") ("dba_col_comments") ("dba_col_privs") ("dba_coll_types") ("dba_common_audit_trail") ("dba_cons_obj_columns") ("dba_constraints") ("dba_context") ("dba_data_files") ("dba_datapump_jobs") ("dba_datapump_sessions") ("dba_db_links") ("dba_ddl_locks") ("dba_dependencies") ("dba_dim_attributes") ("dba_dim_child_of") ("dba_dim_hierarchies") ("dba_dim_join_key") ("dba_dim_level_key") ("dba_dim_levels") ("dba_dimensions") ("dba_directories") ("dba_dml_locks") ("dba_dmt_free_space") ("dba_dmt_used_extents") ("dba_enabled_aggregations") ("dba_enabled_traces") ("dba_errors") ("dba_evaluation_context_tables") ("dba_evaluation_context_vars") ("dba_evaluation_contexts") ("dba_exp_files") ("dba_exp_objects") ("dba_exp_version") ("dba_extents") ("dba_external_locations") ("dba_external_tables") ("dba_feature_usage_statistics") ("dba_fga_audit_trail") ("dba_free_space") ("dba_free_space_coalesced") ("dba_high_water_mark_statistics") ("dba_hist_active_sess_history") ("dba_hist_baseline") ("dba_hist_bg_event_summary") ("dba_hist_buffer_pool_stat") ("dba_hist_cr_block_server") ("dba_hist_current_block_server") ("dba_hist_database_instance") ("dba_hist_datafile") ("dba_hist_db_cache_advice") ("dba_hist_dlm_misc") ("dba_hist_enqueue_stat") ("dba_hist_event_name") ("dba_hist_filemetric_history") ("dba_hist_filestatxs") ("dba_hist_instance_recovery") ("dba_hist_java_pool_advice") ("dba_hist_latch") ("dba_hist_latch_children") ("dba_hist_latch_misses_summary") ("dba_hist_latch_name") ("dba_hist_latch_parent") ("dba_hist_librarycache") ("dba_hist_log") ("dba_hist_metric_name") ("dba_hist_mttr_target_advice") ("dba_hist_optimizer_env") ("dba_hist_osstat") ("dba_hist_osstat_name") ("dba_hist_parameter") ("dba_hist_parameter_name") ("dba_hist_pga_target_advice") ("dba_hist_pgastat") ("dba_hist_resource_limit") ("dba_hist_rollstat") ("dba_hist_rowcache_summary") ("dba_hist_seg_stat") ("dba_hist_seg_stat_obj") ("dba_hist_service_name") ("dba_hist_service_stat") ("dba_hist_service_wait_class") ("dba_hist_sessmetric_history") ("dba_hist_sga") ("dba_hist_sgastat") ("dba_hist_shared_pool_advice") ("dba_hist_snap_error") ("dba_hist_snapshot") ("dba_hist_sql_plan") ("dba_hist_sql_summary") ("dba_hist_sql_workarea_hstgrm") ("dba_hist_sqlbind") ("dba_hist_sqlstat") ("dba_hist_sqltext") ("dba_hist_stat_name") ("dba_hist_sys_time_model") ("dba_hist_sysmetric_history") ("dba_hist_sysmetric_summary") ("dba_hist_sysstat") ("dba_hist_system_event") ("dba_hist_tablespace_stat") ("dba_hist_tbspc_space_usage") ("dba_hist_tempfile") ("dba_hist_tempstatxs") ("dba_hist_thread") ("dba_hist_undostat") ("dba_hist_waitclassmet_history") ("dba_hist_waitstat") ("dba_hist_wr_control") ("dba_ind_columns") ("dba_ind_expressions") ("dba_ind_partitions") ("dba_ind_statistics") ("dba_ind_subpartitions") ("dba_indexes") ("dba_indextype_arraytypes") ("dba_indextype_comments") ("dba_indextype_operators") ("dba_indextypes") ("dba_internal_triggers") ("dba_java_arguments") ("dba_java_classes") ("dba_java_derivations") ("dba_java_fields") ("dba_java_implements") ("dba_java_inners") ("dba_java_layouts") ("dba_java_methods") ("dba_java_ncomps") ("dba_java_policy") ("dba_java_resolvers") ("dba_java_throws") ("dba_jobs") ("dba_jobs_running") ("dba_join_ind_columns") ("dba_kgllock") ("dba_libraries") ("dba_lmt_free_space") ("dba_lmt_used_extents") ("dba_lob_partitions") ("dba_lob_subpartitions") ("dba_lob_templates") ("dba_lobs") ("dba_lock") ("dba_lock_internal") ("dba_locks") ("dba_log_group_columns") ("dba_log_groups") ("dba_logmnr_log") ("dba_logmnr_purged_log") ("dba_logmnr_session") ("dba_logstdby_events") ("dba_logstdby_log") ("dba_logstdby_not_unique") ("dba_logstdby_parameters") ("dba_logstdby_progress") ("dba_logstdby_skip") ("dba_logstdby_skip_transaction") ("dba_logstdby_unsupported") ("dba_method_params") ("dba_method_results") ("dba_mview_aggregates") ("dba_mview_analysis") ("dba_mview_comments") ("dba_mview_detail_relations") ("dba_mview_joins") ("dba_mview_keys") ("dba_mview_log_filter_cols") ("dba_mview_logs") ("dba_mview_refresh_times") ("dba_mviews") ("dba_nested_table_cols") ("dba_nested_tables") ("dba_obj_audit_opts") ("dba_obj_colattrs") ("dba_object_size") ("dba_object_tables") ("dba_objects") ("dba_opancillary") ("dba_oparguments") ("dba_opbindings") ("dba_operator_comments") ("dba_operators") ("dba_orphan_key_table") ("dba_outline_hints") ("dba_outlines") ("dba_outstanding_alerts") ("dba_part_col_statistics") ("dba_part_histograms") ("dba_part_indexes") ("dba_part_key_columns") ("dba_part_lobs") ("dba_part_tables") ("dba_partial_drop_tabs") ("dba_pending_conv_tables") ("dba_pending_transactions") ("dba_plsql_object_settings") ("dba_policies") ("dba_policy_contexts") ("dba_policy_groups") ("dba_priv_audit_opts") ("dba_procedures") ("dba_profiles") ("dba_propagation") ("dba_proxies") ("dba_published_columns") ("dba_queue_schedules") ("dba_queue_tables") ("dba_queues") ("dba_rchild") ("dba_recyclebin") ("dba_redefinition_errors") ("dba_redefinition_objects") ("dba_refresh") ("dba_refresh_children") ("dba_refs") ("dba_registered_archived_log") ("dba_registered_mview_groups") ("dba_registered_mviews") ("dba_registry") ("dba_registry_hierarchy") ("dba_repair_table") ("dba_resumable") ("dba_rewrite_equivalences") ("dba_rgroup") ("dba_role_privs") ("dba_roles") ("dba_rollback_segs") ("dba_rsrc_consumer_group_privs") ("dba_rsrc_consumer_groups") ("dba_rsrc_group_mappings") ("dba_rsrc_manager_system_privs") ("dba_rsrc_mapping_priority") ("dba_rsrc_plan_directives") ("dba_rsrc_plans") ("dba_rule_set_rules") ("dba_rule_sets") ("dba_rules") ("dba_scheduler_job_args") ("dba_scheduler_job_classes") ("dba_scheduler_job_log") ("dba_scheduler_job_run_details") ("dba_scheduler_jobs") ("dba_scheduler_program_args") ("dba_scheduler_programs") ("dba_scheduler_running_jobs") ("dba_scheduler_schedules") ("dba_scheduler_window_details") ("dba_scheduler_window_groups") ("dba_scheduler_window_log") ("dba_scheduler_windows") ("dba_scheduler_wingroup_members") ("dba_sec_relevant_cols") ("dba_segments") ("dba_sequences") ("dba_server_registry") ("dba_services") ("dba_source") ("dba_source_tables") ("dba_sql_profiles") ("dba_sqlj_type_attrs") ("dba_sqlj_type_methods") ("dba_sqlj_types") ("dba_sqlset") ("dba_sqlset_binds") ("dba_sqlset_references") ("dba_sqlset_statements") ("dba_sqltune_binds") ("dba_sqltune_plans") ("dba_sqltune_rationale_plan") ("dba_sqltune_statistics") ("dba_stmt_audit_opts") ("dba_stored_settings") ("dba_streams_administrator") ("dba_streams_global_rules") ("dba_streams_message_consumers") ("dba_streams_message_rules") ("dba_streams_newly_supported") ("dba_streams_rules") ("dba_streams_schema_rules") ("dba_streams_table_rules") ("dba_streams_transform_function") ("dba_streams_unsupported") ("dba_subpart_col_statistics") ("dba_subpart_histograms") ("dba_subpart_key_columns") ("dba_subpartition_templates") ("dba_subscribed_columns") ("dba_subscribed_tables") ("dba_subscriptions") ("dba_synonyms") ("dba_sys_privs") ("dba_tab_col_statistics") ("dba_tab_cols") ("dba_tab_columns") ("dba_tab_comments") ("dba_tab_histograms") ("dba_tab_modifications") ("dba_tab_partitions") ("dba_tab_privs") ("dba_tab_statistics") ("dba_tab_subpartitions") ("dba_tables") ("dba_tablespace_groups") ("dba_tablespaces") ("dba_temp_files") ("dba_thresholds") ("dba_transformations") ("dba_trigger_cols") ("dba_triggers") ("dba_ts_quotas") ("dba_tune_mview") ("dba_type_attrs") ("dba_type_methods") ("dba_type_versions") ("dba_types") ("dba_undo_extents") ("dba_unused_col_tabs") ("dba_updatable_columns") ("dba_users") ("dba_ustats") ("dba_varrays") ("dba_views") ("dba_waiters") ("dba_warning_settings") ("dba_xml_schemas") ("dba_xml_tab_cols") ("dba_xml_tables") ("dba_xml_view_cols") ("flashback_transaction_query") ("global_name") ("hs_all_caps") ("hs_all_dd") ("dba_xml_views") ("dbms_alert_info") ("dbms_lock_allocated") ("deptree") ("dict") ("dict_columns") ("dictionary") ("error_size") ("exceptions") ("hs_all_inits") ("hs_base_caps") ("hs_base_dd") ("hs_class_caps") ("hs_class_dd") ("hs_class_init") ("hs_external_object_privileges") ("hs_external_objects") ("hs_external_user_privileges") ("hs_fds_class") ("hs_fds_inst") ("hs_inst_caps") ("hs_inst_dd") ("hs_inst_init") ("ideptree") ("ind") ("index_histogram") ("index_stats") ("map_object") ("nls_database_parameters") ("nls_instance_parameters") ("nls_session_parameters") ("obj") ("plan_table") ("pluggable_set_check") ("product_component_version") ("proxy_users") ("pstubtbl") ("public_dependency") ("publicsyn") ("queue_privileges") ("recyclebin") ("resource_cost") ("resource_map") ("role_role_privs") ("role_sys_privs") ("role_tab_privs") ("seq") ("session_context") ("session_privs") ("session_roles") ("source_size") ("stmt_audit_option_map") ("syn") ("synonyms") ("syscatalog") ("sysfiles") ("syssegobj") ("system_privilege_map") ("sys_objects") ("tab") ("table_privilege_map") ("tabs") ("tabquotas") ("trusted_servers") ("ts_pitr_check") ("ts_pitr_objects_to_be_dropped") ("uni_pluggable_set_check") ("user_advisor_actions") ("user_advisor_findings") ("user_advisor_journal") ("user_advisor_log") ("user_advisor_objects") ("user_advisor_parameters") ("user_advisor_rationale") ("user_advisor_recommendations") ("user_advisor_sqla_rec_sum") ("user_advisor_sqla_wk_map") ("user_advisor_sqla_wk_stmts") ("user_advisor_sqlw_journal") ("user_advisor_sqlw_parameters") ("user_advisor_sqlw_stmts") ("user_advisor_sqlw_sum") ("user_advisor_sqlw_tables") ("user_advisor_sqlw_templates") ("user_advisor_tasks") ("user_advisor_templates") ("user_all_tables") ("user_aq_agent_privs") ("user_arguments") ("user_associations") ("user_attribute_transformations") ("user_audit_object") ("user_audit_policies") ("user_audit_policy_columns") ("user_audit_session") ("user_audit_statement") ("user_audit_trail") ("user_aw_ps") ("user_aws") ("user_base_table_mviews") ("user_catalog") ("user_clu_columns") ("user_cluster_hash_expressions") ("user_clusters") ("user_col_comments") ("user_col_privs") ("user_col_privs_made") ("user_col_privs_recd") ("user_coll_types") ("user_cons_columns") ("user_cons_obj_columns") ("user_constraints") ("user_datapump_jobs") ("user_db_links") ("user_dependencies") ("user_dim_attributes") ("user_dim_child_of") ("user_dim_hierarchies") ("user_dim_join_key") ("user_dim_level_key") ("user_dim_levels") ("user_dimensions") ("user_errors") ("user_evaluation_context_tables") ("user_evaluation_context_vars") ("user_evaluation_contexts") ("user_extents") ("user_external_locations") ("user_external_tables") ("user_free_space") ("user_ind_columns") ("user_ind_expressions") ("user_ind_partitions") ("user_ind_statistics") ("user_ind_subpartitions") ("user_indexes") ("user_indextype_arraytypes") ("user_indextype_comments") ("user_indextype_operators") ("user_indextypes") ("user_internal_triggers") ("user_java_arguments") ("user_java_classes") ("user_java_derivations") ("user_java_fields") ("user_java_implements") ("user_java_inners") ("user_java_layouts") ("user_java_methods") ("user_java_ncomps") ("user_java_policy") ("user_java_resolvers") ("user_java_throws") ("user_jobs") ("user_join_ind_columns") ("user_libraries") ("user_lob_partitions") ("user_lob_subpartitions") ("user_lob_templates") ("user_lobs") ("user_log_group_columns") ("user_log_groups") ("user_method_params") ("user_method_results") ("user_mview_aggregates") ("user_mview_analysis") ("user_mview_comments") ("user_mview_detail_relations") ("user_mview_joins") ("user_mview_keys") ("user_mview_logs") ("user_mview_refresh_times") ("user_mviews") ("user_nested_table_cols") ("user_nested_tables") ("user_obj_audit_opts") ("user_obj_colattrs") ("user_object_size") ("user_object_tables") ("user_objects") ("user_opancillary") ("user_oparguments") ("user_opbindings") ("user_operator_comments") ("user_operators") ("user_outline_hints") ("user_outlines") ("user_part_col_statistics") ("user_part_histograms") ("user_part_indexes") ("user_part_key_columns") ("user_part_lobs") ("user_part_tables") ("user_partial_drop_tabs") ("user_password_limits") ("user_pending_conv_tables") ("user_plsql_object_settings") ("user_policies") ("user_policy_contexts") ("user_policy_groups") ("user_procedures") ("user_proxies") ("user_published_columns") ("user_queue_schedules") ("user_queue_tables") ("user_queues") ("user_recyclebin") ("user_refresh") ("user_refresh_children") ("user_refs") ("user_registered_mviews") ("user_registry") ("user_resource_limits") ("user_resumable") ("user_rewrite_equivalences") ("user_role_privs") ("user_rsrc_consumer_group_privs") ("user_rsrc_manager_system_privs") ("user_rule_set_rules") ("user_rule_sets") ("user_rules") ("user_scheduler_job_args") ("user_scheduler_job_log") ("user_scheduler_job_run_details") ("user_scheduler_jobs") ("user_scheduler_program_args") ("user_scheduler_programs") ("user_scheduler_running_jobs") ("user_scheduler_schedules") ("user_sec_relevant_cols") ("user_segments") ("user_sequences") ("user_source") ("user_source_tables") ("user_sqlj_type_attrs") ("user_sqlj_type_methods") ("user_sqlj_types") ("user_sqlset") ("user_sqlset_binds") ("user_sqlset_references") ("user_sqlset_statements") ("user_sqltune_binds") ("user_sqltune_plans") ("user_sqltune_rationale_plan") ("user_sqltune_statistics") ("user_stored_settings") ("user_subpart_col_statistics") ("user_subpart_histograms") ("user_subpart_key_columns") ("user_subpartition_templates") ("user_subscribed_columns") ("user_subscribed_tables") ("user_subscriptions") ("user_synonyms") ("user_sys_privs") ("user_tab_col_statistics") ("user_tab_cols") ("user_tab_columns") ("user_tab_comments") ("user_tab_histograms") ("user_tab_modifications") ("user_tab_partitions") ("user_tab_privs") ("user_tab_privs_made") ("user_tab_privs_recd") ("user_tab_statistics") ("user_tab_subpartitions") ("user_tables") ("user_tablespaces") ("user_transformations") ("user_trigger_cols") ("user_triggers") ("user_ts_quotas") ("user_tune_mview") ("user_type_attrs") ("user_type_methods") ("user_type_versions") ("user_types") ("user_unused_col_tabs") ("user_updatable_columns") ("user_users") ("user_ustats") ("user_varrays") ("user_views") ("user_warning_settings") ("user_xml_schemas") ("user_xml_tab_cols") ("user_xml_tables") ("user_xml_view_cols") ("user_xml_views") ("v$access") ("v$active_instances") ("v$active_services") ("v$active_sess_pool_mth") ("v$active_session_history") ("v$alert_types") ("v$aq") ("v$archive") ("v$archive_dest") ("v$archive_dest_status") ("v$archive_gap") ("v$archive_processes") ("v$archived_log") ("v$asm_alias") ("v$asm_client") ("v$asm_disk") ("v$asm_diskgroup") ("v$asm_file") ("v$asm_operation") ("v$asm_template") ("v$backup") ("v$backup_async_io") ("v$backup_corruption") ("v$backup_datafile") ("v$backup_device") ("v$backup_files") ("v$backup_piece") ("v$backup_redolog") ("v$backup_set") ("v$backup_spfile") ("v$backup_sync_io") ("v$bgprocess") ("v$bh") ("v$block_change_tracking") ("v$buffer_pool") ("v$buffer_pool_statistics") ("v$buffered_publishers") ("v$buffered_queues") ("v$buffered_subscribers") ("v$cache") ("v$cache_lock") ("v$cache_transfer") ("v$circuit") ("v$class_cache_transfer") ("v$client_stats") ("v$context") ("v$controlfile") ("v$controlfile_record_section") ("v$copy_corruption") ("v$cr_block_server") ("v$current_block_server") ("v$database") ("v$database_block_corruption") ("v$database_incarnation") ("v$datafile") ("v$datafile_copy") ("v$datafile_header") ("v$dataguard_config") ("v$dataguard_status") ("v$db_cache_advice") ("v$db_object_cache") ("v$db_pipes") ("v$dbfile") ("v$dblink") ("v$deleted_object") ("v$dispatcher") ("v$dispatcher_config") ("v$dispatcher_rate") ("v$enabledprivs") ("v$enqueue_lock") ("v$enqueue_stat") ("v$event_histogram") ("v$event_name") ("v$eventmetric") ("v$execution") ("v$false_ping") ("v$fast_start_servers") ("v$fast_start_transactions") ("v$file_cache_transfer") ("v$file_histogram") ("v$filemetric") ("v$filemetric_history") ("v$filestat") ("v$fixed_table") ("v$fixed_view_definition") ("v$flashback_database_log") ("v$flashback_database_stat") ("v$gc_element") ("v$gc_elements_with_collisions") ("v$gcshvmaster_info") ("v$gcspfmaster_info") ("v$ges_blocking_enqueue") ("v$ges_convert_local") ("v$ges_convert_remote") ("v$ges_enqueue") ("v$ges_latch") ("v$ges_resource") ("v$ges_statistics") ("v$global_blocked_locks") ("v$global_transaction") ("v$hs_agent") ("v$hs_parameter") ("v$hs_session") ("v$hvmaster_info") ("v$indexed_fixed_column") ("v$instance") ("v$instance_recovery") ("v$java_library_cache_memory") ("v$java_pool_advice") ("v$latch") ("v$latch_children") ("v$latch_misses") ("v$latch_parent") ("v$latchholder") ("v$latchname") ("v$library_cache_memory") ("v$librarycache") ("v$license") ("v$loadistat") ("v$loadpstat") ("v$lock") ("v$lock_activity") ("v$locked_object") ("v$log") ("v$log_history") ("v$logfile") ("v$loghist") ("v$logmnr_contents") ("v$logmnr_dictionary") ("v$logmnr_logs") ("v$logmnr_parameters") ("v$logstdby") ("v$logstdby_stats") ("v$managed_standby") ("v$map_comp_list") ("v$map_element") ("v$map_ext_element") ("v$map_file") ("v$map_file_extent") ("v$map_file_io_stack") ("v$map_library") ("v$map_subelement") ("v$metricname") ("v$mttr_target_advice") ("v$mvrefresh") ("v$mystat") ("v$nls_parameters") ("v$nls_valid_values") ("v$object_dependency") ("v$object_usage") ("v$obsolete_backup_files") ("v$obsolete_parameter") ("v$offline_range") ("v$open_cursor") ("v$option") ("v$osstat") ("v$parallel_degree_limit_mth") ("v$parameter") ("v$parameter2") ("v$pga_target_advice") ("v$pga_target_advice_histogram") ("v$pgastat") ("v$pq_sesstat") ("v$pq_slave") ("v$pq_sysstat") ("v$pq_tqstat") ("v$process") ("v$propagation_receiver") ("v$propagation_sender") ("v$proxy_archivedlog") ("v$proxy_datafile") ("v$pwfile_users") ("v$px_process") ("v$px_process_sysstat") ("v$px_session") ("v$px_sesstat") ("v$queue") ("v$queueing_mth") ("v$recover_file") ("v$recovery_file_dest") ("v$recovery_file_status") ("v$recovery_log") ("v$recovery_progress") ("v$recovery_status") ("v$replprop") ("v$replqueue") ("v$reqdist") ("v$reserved_words") ("v$resource") ("v$resource_limit") ("v$rman_configuration") ("v$rman_output") ("v$rman_status") ("v$rollname") ("v$rollstat") ("v$rowcache") ("v$rowcache_parent") ("v$rowcache_subordinate") ("v$rsrc_consumer_group") ("v$rsrc_consumer_group_cpu_mth") ("v$rsrc_plan") ("v$rsrc_plan_cpu_mth") ("v$rule") ("v$rule_set") ("v$rule_set_aggregate_stats") ("v$segment_statistics") ("v$segstat") ("v$segstat_name") ("v$serv_mod_act_stats") ("v$service_event") ("v$service_stats") ("v$service_wait_class") ("v$servicemetric") ("v$servicemetric_history") ("v$services") ("v$ses_optimizer_env") ("v$sess_io") ("v$sess_time_model") ("v$session") ("v$session_connect_info") ("v$session_cursor_cache") ("v$session_event") ("v$session_longops") ("v$session_object_cache") ("v$session_wait") ("v$session_wait_class") ("v$session_wait_history") ("v$sessmetric") ("v$sesstat") ("v$sga") ("v$sga_current_resize_ops") ("v$sga_dynamic_components") ("v$sga_dynamic_free_memory") ("v$sga_resize_ops") ("v$sgainfo") ("v$sgastat") ("v$shared_pool_advice") ("v$shared_pool_reserved") ("v$shared_server") ("v$shared_server_monitor") ("v$sort_segment") ("v$spparameter") ("v$sql") ("v$sql_bind_capture") ("v$sql_bind_data") ("v$sql_bind_metadata") ("v$sql_cursor") ("v$sql_optimizer_env") ("v$sql_plan") ("v$sql_plan_statistics") ("v$sql_plan_statistics_all") ("v$sql_redirection") ("v$sql_shared_cursor") ("v$sql_shared_memory") ("v$sql_workarea") ("v$sql_workarea_active") ("v$sql_workarea_histogram") ("v$sqlarea") ("v$sqltext") ("v$sqltext_with_newlines") ("v$standby_log") ("v$statistics_level") ("v$statname") ("v$streams_apply_coordinator") ("v$streams_apply_reader") ("v$streams_apply_server") ("v$streams_capture") ("v$subcache") ("v$sys_optimizer_env") ("v$sys_time_model") ("v$sysaux_occupants") ("v$sysmetric") ("v$sysmetric_history") ("v$sysmetric_summary") ("v$sysstat") ("v$system_cursor_cache") ("v$system_event") ("v$system_parameter") ("v$system_parameter2") ("v$system_wait_class") ("v$tablespace") ("v$temp_cache_transfer") ("v$temp_extent_map") ("v$temp_extent_pool") ("v$temp_histogram") ("v$temp_space_header") ("v$tempfile") ("v$temporary_lobs") ("v$tempseg_usage") ("v$tempstat") ("v$thread") ("v$threshold_types") ("v$timer") ("v$timezone_names") ("v$transaction") ("v$transaction_enqueue") ("v$transportable_platform") ("v$type_size") ("v$undostat") ("v$version") ("v$vpd_policy") ("v$waitclassmetric") ("v$waitclassmetric_history") ("v$waitstat"))
  "Oracle data dictionary and v$views.")

(setq sql-oracle-ddviews '("all_all_tables" "all_apply" "all_apply_conflict_columns" "all_apply_dml_handlers" "all_apply_enqueue" "all_apply_error" "all_apply_execute" "all_apply_key_columns" "all_apply_parameters" "all_apply_progress" "all_apply_table_columns" "all_arguments" "all_associations" "all_audit_policies" "all_audit_policy_columns" "all_aw_ps" "all_aws" "all_base_table_mviews" "all_capture" "all_capture_extra_attributes" "all_capture_parameters" "all_capture_prepared_database" "all_capture_prepared_schemas" "all_capture_prepared_tables" "all_catalog" "all_cluster_hash_expressions" "all_clusters" "all_col_comments" "all_col_privs" "all_col_privs_made" "all_col_privs_recd" "all_coll_types" "all_cons_columns" "all_cons_obj_columns" "all_constraints" "all_context" "all_db_links" "all_def_audit_opts" "all_dependencies" "all_dim_attributes" "all_dim_child_of" "all_dim_hierarchies" "all_dim_join_key" "all_dim_level_key" "all_dim_levels" "all_dimensions" "all_directories" "all_errors" "all_evaluation_context_tables" "all_evaluation_context_vars" "all_evaluation_contexts" "all_external_locations" "all_external_tables" "all_ind_columns" "all_ind_expressions" "all_ind_partitions" "all_ind_statistics" "all_ind_subpartitions" "all_indexes" "all_indextype_arraytypes" "all_indextype_comments" "all_indextype_operators" "all_indextypes" "all_internal_triggers" "all_java_arguments" "all_java_classes" "all_java_derivations" "all_java_fields" "all_java_implements" "all_java_inners" "all_java_layouts" "all_java_methods" "all_java_ncomps" "all_java_resolvers" "all_java_throws" "all_jobs" "all_join_ind_columns" "all_libraries" "all_lob_partitions" "all_lob_subpartitions" "all_lob_templates" "all_lobs" "all_log_group_columns" "all_log_groups" "all_method_params" "all_method_results" "all_mview_aggregates" "all_mview_analysis" "all_mview_comments" "all_mview_detail_relations" "all_mview_joins" "all_mview_keys" "all_mview_logs" "all_mview_refresh_times" "all_mviews" "all_nested_table_cols" "all_nested_tables" "all_obj_colattrs" "all_object_tables" "all_objects" "all_opancillary" "all_oparguments" "all_opbindings" "all_operator_comments" "all_operators" "all_outline_hints" "all_outlines" "all_part_col_statistics" "all_part_histograms" "all_part_indexes" "all_part_key_columns" "all_part_lobs" "all_part_tables" "all_partial_drop_tabs" "all_pending_conv_tables" "all_plsql_object_settings" "all_policies" "all_policy_contexts" "all_policy_groups" "all_procedures" "all_propagation" "all_published_columns" "all_queue_tables" "all_queues" "all_refresh" "all_refresh_children" "all_refresh_dependencies" "all_refs" "all_registered_mviews" "all_registry_banners" "all_rewrite_equivalences" "all_rule_set_rules" "all_rule_sets" "all_rules" "all_scheduler_job_args" "all_scheduler_job_classes" "all_scheduler_job_log" "all_scheduler_job_run_details" "all_scheduler_jobs" "all_scheduler_program_args" "all_scheduler_programs" "all_scheduler_running_jobs" "all_scheduler_schedules" "all_scheduler_window_details" "all_scheduler_window_groups" "all_scheduler_window_log" "all_scheduler_windows" "all_scheduler_wingroup_members" "all_sec_relevant_cols" "all_sequences" "all_services" "all_source" "all_source_tables" "all_sqlj_type_attrs" "all_sqlj_type_methods" "all_sqlj_types" "all_stored_settings" "all_streams_global_rules" "all_streams_message_consumers" "all_streams_message_rules" "all_streams_newly_supported" "all_streams_rules" "all_streams_schema_rules" "all_streams_table_rules" "all_streams_transform_function" "all_streams_unsupported" "all_subpart_col_statistics" "all_subpart_histograms" "all_subpart_key_columns" "all_subpartition_templates" "all_subscribed_columns" "all_subscribed_tables" "all_subscriptions" "all_sumdelta" "all_synonyms" "all_tab_col_statistics" "all_tab_cols" "all_tab_columns" "all_tab_comments" "all_tab_histograms" "all_tab_modifications" "all_tab_partitions" "all_tab_privs" "all_tab_privs_made" "all_tab_privs_recd" "all_tab_statistics" "all_tab_subpartitions" "all_tables" "all_trigger_cols" "all_triggers" "all_type_attrs" "all_type_methods" "all_type_versions" "all_types" "all_unused_col_tabs" "all_updatable_columns" "all_users" "all_ustats" "all_varrays" "all_views" "all_warning_settings" "all_xml_schemas" "all_xml_tab_cols" "all_xml_tables" "all_xml_view_cols" "all_xml_views" "audit_actions" "catalog" "cat" "chained_rows" "change_sets" "change_sources" "change_tables" "clu" "col" "cols" "database_properties" "dba_2pc_neighbors" "dba_2pc_pending" "dba_advisor_actions" "dba_advisor_commands" "dba_advisor_definitions" "dba_advisor_findings" "dba_advisor_journal" "dba_advisor_log" "dba_advisor_object_types" "dba_advisor_objects" "dba_advisor_parameters" "dba_advisor_rationale" "dba_advisor_recommendations" "dba_advisor_sqla_rec_sum" "dba_advisor_sqla_wk_map" "dba_advisor_sqla_wk_stmts" "dba_advisor_sqlw_journal" "dba_advisor_sqlw_parameters" "dba_advisor_sqlw_stmts" "dba_advisor_sqlw_sum" "dba_advisor_sqlw_tables" "dba_advisor_sqlw_templates" "dba_advisor_tasks" "dba_advisor_templates" "dba_advisor_usage" "dba_alert_history" "dba_all_tables" "dba_application_roles" "dba_apply" "dba_apply_conflict_columns" "dba_apply_dml_handlers" "dba_apply_enqueue" "dba_apply_error" "dba_apply_execute" "dba_apply_instantiated_global" "dba_apply_instantiated_objects" "dba_apply_instantiated_schemas" "dba_apply_key_columns" "dba_apply_parameters" "dba_apply_progress" "dba_apply_table_columns" "dba_aq_agent_privs" "dba_aq_agents" "dba_associations" "dba_attribute_transformations" "dba_audit_exists" "dba_audit_object" "dba_audit_policies" "dba_audit_policy_columns" "dba_audit_session" "dba_audit_statement" "dba_audit_trail" "dba_aw_ps" "dba_aws" "dba_base_table_mviews" "dba_blockers" "dba_capture" "dba_capture_extra_attributes" "dba_capture_parameters" "dba_capture_prepared_database" "dba_capture_prepared_schemas" "dba_capture_prepared_tables" "dba_catalog" "dba_clu_columns" "dba_cluster_hash_expressions" "dba_clusters" "dba_col_comments" "dba_col_privs" "dba_coll_types" "dba_common_audit_trail" "dba_cons_obj_columns" "dba_constraints" "dba_context" "dba_data_files" "dba_datapump_jobs" "dba_datapump_sessions" "dba_db_links" "dba_ddl_locks" "dba_dependencies" "dba_dim_attributes" "dba_dim_child_of" "dba_dim_hierarchies" "dba_dim_join_key" "dba_dim_level_key" "dba_dim_levels" "dba_dimensions" "dba_directories" "dba_dml_locks" "dba_dmt_free_space" "dba_dmt_used_extents" "dba_enabled_aggregations" "dba_enabled_traces" "dba_errors" "dba_evaluation_context_tables" "dba_evaluation_context_vars" "dba_evaluation_contexts" "dba_exp_files" "dba_exp_objects" "dba_exp_version" "dba_extents" "dba_external_locations" "dba_external_tables" "dba_feature_usage_statistics" "dba_fga_audit_trail" "dba_free_space" "dba_free_space_coalesced" "dba_high_water_mark_statistics" "dba_hist_active_sess_history" "dba_hist_baseline" "dba_hist_bg_event_summary" "dba_hist_buffer_pool_stat" "dba_hist_cr_block_server" "dba_hist_current_block_server" "dba_hist_database_instance" "dba_hist_datafile" "dba_hist_db_cache_advice" "dba_hist_dlm_misc" "dba_hist_enqueue_stat" "dba_hist_event_name" "dba_hist_filemetric_history" "dba_hist_filestatxs" "dba_hist_instance_recovery" "dba_hist_java_pool_advice" "dba_hist_latch" "dba_hist_latch_children" "dba_hist_latch_misses_summary" "dba_hist_latch_name" "dba_hist_latch_parent" "dba_hist_librarycache" "dba_hist_log" "dba_hist_metric_name" "dba_hist_mttr_target_advice" "dba_hist_optimizer_env" "dba_hist_osstat" "dba_hist_osstat_name" "dba_hist_parameter" "dba_hist_parameter_name" "dba_hist_pga_target_advice" "dba_hist_pgastat" "dba_hist_resource_limit" "dba_hist_rollstat" "dba_hist_rowcache_summary" "dba_hist_seg_stat" "dba_hist_seg_stat_obj" "dba_hist_service_name" "dba_hist_service_stat" "dba_hist_service_wait_class" "dba_hist_sessmetric_history" "dba_hist_sga" "dba_hist_sgastat" "dba_hist_shared_pool_advice" "dba_hist_snap_error" "dba_hist_snapshot" "dba_hist_sql_plan" "dba_hist_sql_summary" "dba_hist_sql_workarea_hstgrm" "dba_hist_sqlbind" "dba_hist_sqlstat" "dba_hist_sqltext" "dba_hist_stat_name" "dba_hist_sys_time_model" "dba_hist_sysmetric_history" "dba_hist_sysmetric_summary" "dba_hist_sysstat" "dba_hist_system_event" "dba_hist_tablespace_stat" "dba_hist_tbspc_space_usage" "dba_hist_tempfile" "dba_hist_tempstatxs" "dba_hist_thread" "dba_hist_undostat" "dba_hist_waitclassmet_history" "dba_hist_waitstat" "dba_hist_wr_control" "dba_ind_columns" "dba_ind_expressions" "dba_ind_partitions" "dba_ind_statistics" "dba_ind_subpartitions" "dba_indexes" "dba_indextype_arraytypes" "dba_indextype_comments" "dba_indextype_operators" "dba_indextypes" "dba_internal_triggers" "dba_java_arguments" "dba_java_classes" "dba_java_derivations" "dba_java_fields" "dba_java_implements" "dba_java_inners" "dba_java_layouts" "dba_java_methods" "dba_java_ncomps" "dba_java_policy" "dba_java_resolvers" "dba_java_throws" "dba_jobs" "dba_jobs_running" "dba_join_ind_columns" "dba_kgllock" "dba_libraries" "dba_lmt_free_space" "dba_lmt_used_extents" "dba_lob_partitions" "dba_lob_subpartitions" "dba_lob_templates" "dba_lobs" "dba_lock" "dba_lock_internal" "dba_locks" "dba_log_group_columns" "dba_log_groups" "dba_logmnr_log" "dba_logmnr_purged_log" "dba_logmnr_session" "dba_logstdby_events" "dba_logstdby_log" "dba_logstdby_not_unique" "dba_logstdby_parameters" "dba_logstdby_progress" "dba_logstdby_skip" "dba_logstdby_skip_transaction" "dba_logstdby_unsupported" "dba_method_params" "dba_method_results" "dba_mview_aggregates" "dba_mview_analysis" "dba_mview_comments" "dba_mview_detail_relations" "dba_mview_joins" "dba_mview_keys" "dba_mview_log_filter_cols" "dba_mview_logs" "dba_mview_refresh_times" "dba_mviews" "dba_nested_table_cols" "dba_nested_tables" "dba_obj_audit_opts" "dba_obj_colattrs" "dba_object_size" "dba_object_tables" "dba_objects" "dba_opancillary" "dba_oparguments" "dba_opbindings" "dba_operator_comments" "dba_operators" "dba_orphan_key_table" "dba_outline_hints" "dba_outlines" "dba_outstanding_alerts" "dba_part_col_statistics" "dba_part_histograms" "dba_part_indexes" "dba_part_key_columns" "dba_part_lobs" "dba_part_tables" "dba_partial_drop_tabs" "dba_pending_conv_tables" "dba_pending_transactions" "dba_plsql_object_settings" "dba_policies" "dba_policy_contexts" "dba_policy_groups" "dba_priv_audit_opts" "dba_procedures" "dba_profiles" "dba_propagation" "dba_proxies" "dba_published_columns" "dba_queue_schedules" "dba_queue_tables" "dba_queues" "dba_rchild" "dba_recyclebin" "dba_redefinition_errors" "dba_redefinition_objects" "dba_refresh" "dba_refresh_children" "dba_refs" "dba_registered_archived_log" "dba_registered_mview_groups" "dba_registered_mviews" "dba_registry" "dba_registry_hierarchy" "dba_repair_table" "dba_resumable" "dba_rewrite_equivalences" "dba_rgroup" "dba_role_privs" "dba_roles" "dba_rollback_segs" "dba_rsrc_consumer_group_privs" "dba_rsrc_consumer_groups" "dba_rsrc_group_mappings" "dba_rsrc_manager_system_privs" "dba_rsrc_mapping_priority" "dba_rsrc_plan_directives" "dba_rsrc_plans" "dba_rule_set_rules" "dba_rule_sets" "dba_rules" "dba_scheduler_job_args" "dba_scheduler_job_classes" "dba_scheduler_job_log" "dba_scheduler_job_run_details" "dba_scheduler_jobs" "dba_scheduler_program_args" "dba_scheduler_programs" "dba_scheduler_running_jobs" "dba_scheduler_schedules" "dba_scheduler_window_details" "dba_scheduler_window_groups" "dba_scheduler_window_log" "dba_scheduler_windows" "dba_scheduler_wingroup_members" "dba_sec_relevant_cols" "dba_segments" "dba_sequences" "dba_server_registry" "dba_services" "dba_source" "dba_source_tables" "dba_sql_profiles" "dba_sqlj_type_attrs" "dba_sqlj_type_methods" "dba_sqlj_types" "dba_sqlset" "dba_sqlset_binds" "dba_sqlset_references" "dba_sqlset_statements" "dba_sqltune_binds" "dba_sqltune_plans" "dba_sqltune_rationale_plan" "dba_sqltune_statistics" "dba_stmt_audit_opts" "dba_stored_settings" "dba_streams_administrator" "dba_streams_global_rules" "dba_streams_message_consumers" "dba_streams_message_rules" "dba_streams_newly_supported" "dba_streams_rules" "dba_streams_schema_rules" "dba_streams_table_rules" "dba_streams_transform_function" "dba_streams_unsupported" "dba_subpart_col_statistics" "dba_subpart_histograms" "dba_subpart_key_columns" "dba_subpartition_templates" "dba_subscribed_columns" "dba_subscribed_tables" "dba_subscriptions" "dba_synonyms" "dba_sys_privs" "dba_tab_col_statistics" "dba_tab_cols" "dba_tab_columns" "dba_tab_comments" "dba_tab_histograms" "dba_tab_modifications" "dba_tab_partitions" "dba_tab_privs" "dba_tab_statistics" "dba_tab_subpartitions" "dba_tables" "dba_tablespace_groups" "dba_tablespaces" "dba_temp_files" "dba_thresholds" "dba_transformations" "dba_trigger_cols" "dba_triggers" "dba_ts_quotas" "dba_tune_mview" "dba_type_attrs" "dba_type_methods" "dba_type_versions" "dba_types" "dba_undo_extents" "dba_unused_col_tabs" "dba_updatable_columns" "dba_users" "dba_ustats" "dba_varrays" "dba_views" "dba_waiters" "dba_warning_settings" "dba_xml_schemas" "dba_xml_tab_cols" "dba_xml_tables" "dba_xml_view_cols" "flashback_transaction_query" "global_name" "hs_all_caps" "hs_all_dd" "dba_xml_views" "dbms_alert_info" "dbms_lock_allocated" "deptree" "dict" "dict_columns" "dictionary" "error_size" "exceptions" "hs_all_inits" "hs_base_caps" "hs_base_dd" "hs_class_caps" "hs_class_dd" "hs_class_init" "hs_external_object_privileges" "hs_external_objects" "hs_external_user_privileges" "hs_fds_class" "hs_fds_inst" "hs_inst_caps" "hs_inst_dd" "hs_inst_init" "ideptree" "ind" "index_histogram" "index_stats" "map_object" "nls_database_parameters" "nls_instance_parameters" "nls_session_parameters" "obj" "plan_table" "pluggable_set_check" "product_component_version" "proxy_users" "pstubtbl" "public_dependency" "publicsyn" "queue_privileges" "recyclebin" "resource_cost" "resource_map" "role_role_privs" "role_sys_privs" "role_tab_privs" "seq" "session_context" "session_privs" "session_roles" "source_size" "stmt_audit_option_map" "syn" "synonyms" "syscatalog" "sysfiles" "syssegobj" "system_privilege_map" "sys_objects" "tab" "table_privilege_map" "tabs" "tabquotas" "trusted_servers" "ts_pitr_check" "ts_pitr_objects_to_be_dropped" "uni_pluggable_set_check" "user_advisor_actions" "user_advisor_findings" "user_advisor_journal" "user_advisor_log" "user_advisor_objects" "user_advisor_parameters" "user_advisor_rationale" "user_advisor_recommendations" "user_advisor_sqla_rec_sum" "user_advisor_sqla_wk_map" "user_advisor_sqla_wk_stmts" "user_advisor_sqlw_journal" "user_advisor_sqlw_parameters" "user_advisor_sqlw_stmts" "user_advisor_sqlw_sum" "user_advisor_sqlw_tables" "user_advisor_sqlw_templates" "user_advisor_tasks" "user_advisor_templates" "user_all_tables" "user_aq_agent_privs" "user_arguments" "user_associations" "user_attribute_transformations" "user_audit_object" "user_audit_policies" "user_audit_policy_columns" "user_audit_session" "user_audit_statement" "user_audit_trail" "user_aw_ps" "user_aws" "user_base_table_mviews" "user_catalog" "user_clu_columns" "user_cluster_hash_expressions" "user_clusters" "user_col_comments" "user_col_privs" "user_col_privs_made" "user_col_privs_recd" "user_coll_types" "user_cons_columns" "user_cons_obj_columns" "user_constraints" "user_datapump_jobs" "user_db_links" "user_dependencies" "user_dim_attributes" "user_dim_child_of" "user_dim_hierarchies" "user_dim_join_key" "user_dim_level_key" "user_dim_levels" "user_dimensions" "user_errors" "user_evaluation_context_tables" "user_evaluation_context_vars" "user_evaluation_contexts" "user_extents" "user_external_locations" "user_external_tables" "user_free_space" "user_ind_columns" "user_ind_expressions" "user_ind_partitions" "user_ind_statistics" "user_ind_subpartitions" "user_indexes" "user_indextype_arraytypes" "user_indextype_comments" "user_indextype_operators" "user_indextypes" "user_internal_triggers" "user_java_arguments" "user_java_classes" "user_java_derivations" "user_java_fields" "user_java_implements" "user_java_inners" "user_java_layouts" "user_java_methods" "user_java_ncomps" "user_java_policy" "user_java_resolvers" "user_java_throws" "user_jobs" "user_join_ind_columns" "user_libraries" "user_lob_partitions" "user_lob_subpartitions" "user_lob_templates" "user_lobs" "user_log_group_columns" "user_log_groups" "user_method_params" "user_method_results" "user_mview_aggregates" "user_mview_analysis" "user_mview_comments" "user_mview_detail_relations" "user_mview_joins" "user_mview_keys" "user_mview_logs" "user_mview_refresh_times" "user_mviews" "user_nested_table_cols" "user_nested_tables" "user_obj_audit_opts" "user_obj_colattrs" "user_object_size" "user_object_tables" "user_objects" "user_opancillary" "user_oparguments" "user_opbindings" "user_operator_comments" "user_operators" "user_outline_hints" "user_outlines" "user_part_col_statistics" "user_part_histograms" "user_part_indexes" "user_part_key_columns" "user_part_lobs" "user_part_tables" "user_partial_drop_tabs" "user_password_limits" "user_pending_conv_tables" "user_plsql_object_settings" "user_policies" "user_policy_contexts" "user_policy_groups" "user_procedures" "user_proxies" "user_published_columns" "user_queue_schedules" "user_queue_tables" "user_queues" "user_recyclebin" "user_refresh" "user_refresh_children" "user_refs" "user_registered_mviews" "user_registry" "user_resource_limits" "user_resumable" "user_rewrite_equivalences" "user_role_privs" "user_rsrc_consumer_group_privs" "user_rsrc_manager_system_privs" "user_rule_set_rules" "user_rule_sets" "user_rules" "user_scheduler_job_args" "user_scheduler_job_log" "user_scheduler_job_run_details" "user_scheduler_jobs" "user_scheduler_program_args" "user_scheduler_programs" "user_scheduler_running_jobs" "user_scheduler_schedules" "user_sec_relevant_cols" "user_segments" "user_sequences" "user_source" "user_source_tables" "user_sqlj_type_attrs" "user_sqlj_type_methods" "user_sqlj_types" "user_sqlset" "user_sqlset_binds" "user_sqlset_references" "user_sqlset_statements" "user_sqltune_binds" "user_sqltune_plans" "user_sqltune_rationale_plan" "user_sqltune_statistics" "user_stored_settings" "user_subpart_col_statistics" "user_subpart_histograms" "user_subpart_key_columns" "user_subpartition_templates" "user_subscribed_columns" "user_subscribed_tables" "user_subscriptions" "user_synonyms" "user_sys_privs" "user_tab_col_statistics" "user_tab_cols" "user_tab_columns" "user_tab_comments" "user_tab_histograms" "user_tab_modifications" "user_tab_partitions" "user_tab_privs" "user_tab_privs_made" "user_tab_privs_recd" "user_tab_statistics" "user_tab_subpartitions" "user_tables" "user_tablespaces" "user_transformations" "user_trigger_cols" "user_triggers" "user_ts_quotas" "user_tune_mview" "user_type_attrs" "user_type_methods" "user_type_versions" "user_types" "user_unused_col_tabs" "user_updatable_columns" "user_users" "user_ustats" "user_varrays" "user_views" "user_warning_settings" "user_xml_schemas" "user_xml_tab_cols" "user_xml_tables" "user_xml_view_cols" "user_xml_views"))

(setq sql-oracle-v$views '("v$access" "v$active_instances" "v$active_services" "v$active_sess_pool_mth" "v$active_session_history" "v$alert_types" "v$aq" "v$archive" "v$archive_dest" "v$archive_dest_status" "v$archive_gap" "v$archive_processes" "v$archived_log" "v$asm_alias" "v$asm_client" "v$asm_disk" "v$asm_diskgroup" "v$asm_file" "v$asm_operation" "v$asm_template" "v$backup" "v$backup_async_io" "v$backup_corruption" "v$backup_datafile" "v$backup_device" "v$backup_files" "v$backup_piece" "v$backup_redolog" "v$backup_set" "v$backup_spfile" "v$backup_sync_io" "v$bgprocess" "v$bh" "v$block_change_tracking" "v$buffer_pool" "v$buffer_pool_statistics" "v$buffered_publishers" "v$buffered_queues" "v$buffered_subscribers" "v$cache" "v$cache_lock" "v$cache_transfer" "v$circuit" "v$class_cache_transfer" "v$client_stats" "v$context" "v$controlfile" "v$controlfile_record_section" "v$copy_corruption" "v$cr_block_server" "v$current_block_server" "v$database" "v$database_block_corruption" "v$database_incarnation" "v$datafile" "v$datafile_copy" "v$datafile_header" "v$dataguard_config" "v$dataguard_status" "v$db_cache_advice" "v$db_object_cache" "v$db_pipes" "v$dbfile" "v$dblink" "v$deleted_object" "v$dispatcher" "v$dispatcher_config" "v$dispatcher_rate" "v$enabledprivs" "v$enqueue_lock" "v$enqueue_stat" "v$event_histogram" "v$event_name" "v$eventmetric" "v$execution" "v$false_ping" "v$fast_start_servers" "v$fast_start_transactions" "v$file_cache_transfer" "v$file_histogram" "v$filemetric" "v$filemetric_history" "v$filestat" "v$fixed_table" "v$fixed_view_definition" "v$flashback_database_log" "v$flashback_database_stat" "v$gc_element" "v$gc_elements_with_collisions" "v$gcshvmaster_info" "v$gcspfmaster_info" "v$ges_blocking_enqueue" "v$ges_convert_local" "v$ges_convert_remote" "v$ges_enqueue" "v$ges_latch" "v$ges_resource" "v$ges_statistics" "v$global_blocked_locks" "v$global_transaction" "v$hs_agent" "v$hs_parameter" "v$hs_session" "v$hvmaster_info" "v$indexed_fixed_column" "v$instance" "v$instance_recovery" "v$java_library_cache_memory" "v$java_pool_advice" "v$latch" "v$latch_children" "v$latch_misses" "v$latch_parent" "v$latchholder" "v$latchname" "v$library_cache_memory" "v$librarycache" "v$license" "v$loadistat" "v$loadpstat" "v$lock" "v$lock_activity" "v$locked_object" "v$log" "v$log_history" "v$logfile" "v$loghist" "v$logmnr_contents" "v$logmnr_dictionary" "v$logmnr_logs" "v$logmnr_parameters" "v$logstdby" "v$logstdby_stats" "v$managed_standby" "v$map_comp_list" "v$map_element" "v$map_ext_element" "v$map_file" "v$map_file_extent" "v$map_file_io_stack" "v$map_library" "v$map_subelement" "v$metricname" "v$mttr_target_advice" "v$mvrefresh" "v$mystat" "v$nls_parameters" "v$nls_valid_values" "v$object_dependency" "v$object_usage" "v$obsolete_backup_files" "v$obsolete_parameter" "v$offline_range" "v$open_cursor" "v$option" "v$osstat" "v$parallel_degree_limit_mth" "v$parameter" "v$parameter2" "v$pga_target_advice" "v$pga_target_advice_histogram" "v$pgastat" "v$pq_sesstat" "v$pq_slave" "v$pq_sysstat" "v$pq_tqstat" "v$process" "v$propagation_receiver" "v$propagation_sender" "v$proxy_archivedlog" "v$proxy_datafile" "v$pwfile_users" "v$px_process" "v$px_process_sysstat" "v$px_session" "v$px_sesstat" "v$queue" "v$queueing_mth" "v$recover_file" "v$recovery_file_dest" "v$recovery_file_status" "v$recovery_log" "v$recovery_progress" "v$recovery_status" "v$replprop" "v$replqueue" "v$reqdist" "v$reserved_words" "v$resource" "v$resource_limit" "v$rman_configuration" "v$rman_output" "v$rman_status" "v$rollname" "v$rollstat" "v$rowcache" "v$rowcache_parent" "v$rowcache_subordinate" "v$rsrc_consumer_group" "v$rsrc_consumer_group_cpu_mth" "v$rsrc_plan" "v$rsrc_plan_cpu_mth" "v$rule" "v$rule_set" "v$rule_set_aggregate_stats" "v$segment_statistics" "v$segstat" "v$segstat_name" "v$serv_mod_act_stats" "v$service_event" "v$service_stats" "v$service_wait_class" "v$servicemetric" "v$servicemetric_history" "v$services" "v$ses_optimizer_env" "v$sess_io" "v$sess_time_model" "v$session" "v$session_connect_info" "v$session_cursor_cache" "v$session_event" "v$session_longops" "v$session_object_cache" "v$session_wait" "v$session_wait_class" "v$session_wait_history" "v$sessmetric" "v$sesstat" "v$sga" "v$sga_current_resize_ops" "v$sga_dynamic_components" "v$sga_dynamic_free_memory" "v$sga_resize_ops" "v$sgainfo" "v$sgastat" "v$shared_pool_advice" "v$shared_pool_reserved" "v$shared_server" "v$shared_server_monitor" "v$sort_segment" "v$spparameter" "v$sql" "v$sql_bind_capture" "v$sql_bind_data" "v$sql_bind_metadata" "v$sql_cursor" "v$sql_optimizer_env" "v$sql_plan" "v$sql_plan_statistics" "v$sql_plan_statistics_all" "v$sql_redirection" "v$sql_shared_cursor" "v$sql_shared_memory" "v$sql_workarea" "v$sql_workarea_active" "v$sql_workarea_histogram" "v$sqlarea" "v$sqltext" "v$sqltext_with_newlines" "v$standby_log" "v$statistics_level" "v$statname" "v$streams_apply_coordinator" "v$streams_apply_reader" "v$streams_apply_server" "v$streams_capture" "v$subcache" "v$sys_optimizer_env" "v$sys_time_model" "v$sysaux_occupants" "v$sysmetric" "v$sysmetric_history" "v$sysmetric_summary" "v$sysstat" "v$system_cursor_cache" "v$system_event" "v$system_parameter" "v$system_parameter2" "v$system_wait_class" "v$tablespace" "v$temp_cache_transfer" "v$temp_extent_map" "v$temp_extent_pool" "v$temp_histogram" "v$temp_space_header" "v$tempfile" "v$temporary_lobs" "v$tempseg_usage" "v$tempstat" "v$thread" "v$threshold_types" "v$timer" "v$timezone_names" "v$transaction" "v$transaction_enqueue" "v$transportable_platform" "v$type_size" "v$undostat" "v$version" "v$vpd_policy" "v$waitclassmetric" "v$waitclassmetric_history" "v$waitstat"))

(setq sql-oracle-nonquoted-identifier "\\<\\([a-z][a-z0-9$#_]\\{0,29\\}\\)\\>")
(setq sql-oracle-dot-regexp "\\s-*\\.\\s-*")
(setq sqlplus-oracle-send-command-regexp "\\([ \t]*;\\)?$")
(setq sql-oracle-whitespace-regexp "\\s-*")
(setq sql-oracle-procedure-parameter-regexp "\\((.*)\\)?")

(setq sql-oracle-dblink-identifier "\\([a-z][a-z0-9$#_.]\\{0,127\\}\\)")

(setq sql-oracle-schema-object-name (concat "\\(\\(" sql-oracle-nonquoted-identifier "\\s-*\\.\\s-*\\)\\{0,2\\}" sql-oracle-nonquoted-identifier
                                            sql-oracle-whitespace-regexp sql-oracle-procedure-parameter-regexp
                                            "\\(\\s-*\\@\\s-*" sql-oracle-dblink-identifier "\\)?\\)"))

(setq sqlplus-oracle-prompt-regexp "^\\(\\(SQL> \\)+\\|[ 0-9][ 0-9][0-9][* \t]\\)")

(setq sql-oracle-sql-statements
       (concat "^\\s-*"
              "\\("
              (regexp-opt '(
                            "analyze" "audit" "call" "comment" "commit" "delete" "grant" "insert"
                            "merge" "noaudit" "purge" "rename" "revoke" "rollback" "savepoint"
                            "truncate" "update"
                            ) nil)
              "\\|\\(dis\\)?associate\\s-+statistics"
              "\\|explain\\s-+plan"
              "\\|flashback\\s-+\\(database\\|table\\)"
              "\\|lock\\s-+table"
              "\\|set\\s-+\\(constraints?\\|role\\|transaction\\)"
              "\\)"
              "\\b"
              ))

(setq sql-oracle-sql-statement-keywords
      (concat "\\b"
              (regexp-opt '(
                            "add"
                            "as"
                            "asc"
                            "before"
                            "cascade"
                            "check"
                            "columns"
                            "constraint"
                            "data"
                            "datafile"
                            "default"
			    "deferrable"
			    "deferred"
                            "desc"
                            "disable"
                            "each"
;                            "drop"
                            "enable"
			    "exclusive"
                            "for"
                            "from"
                            "having"
                            "in"
			    "initially"
                            "into"
                            "is"
                            "log"
                            "logfile"
                            "map"
                            "member"
			    "mode"
                            "modify"
                            "nested"
			    "nowait"
                            "object"
                            "offline"
                            "on"
                            "online"
                            "out"
                            "references"
                            "returning"
                            "reuse"
                            "row"
                            "select"
			    "set"
                            "size"
                            "supplemental"
                            "switch"
;                            "tablespace"
                            "temporary"
                            "to"
                            "unique"
                            "values"
                            ) t)
              "\\b"))
      
(setq sql-oracle-sql-statement-keyword-regexp
      (concat "\\b"
              "\\("
              "\\(primary\\|foreign\\)\\s-+key"
              "\\|"
              "where\\(\\s-+current\\s-+off\\)?"
              "\\|"
              "\\(group\\|identified\\|order\\|connect\\)\\s-+by"
              "\\|"
              "controlfile\\s-+\\(reuse\\)?"
              "\\|"
              "group\\s-+[0-9]+"
              "\\|"
              "start\\s-+with"
              "\\|"
              "on\\s-+delete\\s-+cascade"
              "\\|"
              "read\\s-+\\(only\\|write\\)"
              "\\|"
              "\\(no\\)?archivelog"
              "\\|"
              "for\\s-+update"
              "\\|"
              "on\\s-+\\(table\\|column\\)"
              "\\)"
              "\\b"))
              
(setq sql-oracle-ddl-keywords-regexp
      (concat "^\\s-*"
              "\\("
              "\\(alter\\|create\\(\\s-+or\\s-+replace\\)?\\|drop\\)"
              "\\s-+"
              "\\("
              (regexp-opt '(
                            "cluster" "context" "controlfile" "dimension"
                            "directory" "diskgroup" "function" "indextype" "java" "library"
                            "operator" "pfile" "procedure" "profile" "role"
                            "sequence" "session" "system" "spfile" "table" "tablespace" "trigger"
                            "user"
                            ) nil)
              "\\|\\(\\(unique\\|bitmap\\)\\s-+\\)?index"
              "\\|\\(materialized\\s-+view\\|snapshot\\)\\(\\s-+log\\s-+on\\)?"
              "\\|schema\\s-+authorization"
              "\\|\\(package\\|type\\)\\(\\s-+body\\)?"
              "\\|\\(public\\s-+\\)?\\(\\(shared\\s-+\\)?\\(public\\s-+\\)?database\\(\\s-+link\\)?\\|\\(private\\s-+\\)?outline\\|rollback\\s-+segment\\|synonym\\)"
              "\\|\\(\\(no\\)?force\\s-+\\)?view"
              "\\)"
              "\\s-+\\)"
              ))

(setq sql-oracle-functions-keywords-regexp
      (concat "\\b"
              (regexp-opt '(
                            "abs" "acos" "add_months" "ascii" "asciistr" "asin"
                            "atan" "atan2" "avg" "bfilename" "bin_to_num" "bitand"
                            "cardinality" "cast" "ceil" "chartorowid" "chr"
                            "coalesce" "collect" "compose" "concat" "convert"
                            "corr" "corr_k" "corr_s" "cos" "cosh" "count" "covar_pop"
                            "covar_samp" "cume_dist" "current_date" "current_timestamp"
                            "cv" "dbtimezone" "decode" "decompose" "dense_rank" "depth"
                            "deref" "dump" "empty_blob" "empty_clob" "existsnode" "exp"
                            "extract" "extractvalue" "first" "floor" "from_tz"
                            "greatest" "grouping" "grouping_id" "group_id" "hextoraw"
                            "initcap" "instr" "iteration_number" "last" "last_day"
                            "least" "length" "listagg" "ln" "lnnvl" "localtimestamp" "log" "lower"
                            "lpad" "ltrim" "make_ref" "max" "median" "min" "mod"
                            "months_between" "nanvl" "new_time" "next_day" "nlssort"
                            "nls_charset_decl_len" "nls_charset_id" "nls_charset_name"
                            "nls_initcap" "nls_lower" "nls_upper" "nullif"
                            "numtodsinterval" "numtoyminterval" "nvl" "nvl2" "ora_hash"
                            "path" "percentile_cont" "percentile_disc" "percent_rank"
                            "power" "powermultiset" "powermultiset_by_cardinality"
                            "presentnnv" "presentv" "previous" "rank" "rawtohex"
                            "rawtonhex" "ref" "reftohex" "regexp_instr" "regexp_replace"
                            "regexp_substr" "regr_ (linear regression) functions"
                            "remainder" "replace" "round" "rowidtochar" "rowidtonchar"
                            "rpad" "rtrim" "scn_to_timestamp" "sessiontimezone" "set"
                            "sign" "sin" "sinh" "soundex" "sqrt" "stats_binomial_test"
                            "stats_crosstab" "stats_f_test" "stats_ks_test" "stats_mode"
                            "stats_mw_test" "stats_one_way_anova" "stats_t_test_indep"
                            "stats_t_test_indepu" "stats_t_test_one" "stats_t_test_paired"
                            "stats_wsr_test" "stddev" "stddev_pop" "stddev_samp" "substr"
                            "sum" "systimestamp" "sys_connect_by_path"
                            "sys_context" "sys_dburigen" "sys_extract_utc" "sys_guid"
                            "sys_typeid" "sys_xmlagg" "sys_xmlgen" "tan" "tanh"
                            "timestamp_to_scn" "to_binary_double" "to_binary_float"
                            "to_char" "to_clob" "to_date" "to_dsinterval" "to_lob"
                            "to_multi_byte" "to_nchar" "to_nclob" "to_number"
                            "to_single_byte" "to_timestamp" "to_timestamp_tz"
                            "to_yminterval" "translate" "treat" "trim" "trunc" "tz_offset"
                            "uid" "unistr" "updatexml" "upper" "user" "userenv" "value"
                            "variance" "var_pop" "var_samp" "vsize" "width_bucket" "xmlagg"
                            "xmlcolattval" "xmlconcat" "xmlforest" "xmlsequence"
                            "xmltransform"
                            ) t)
              "\\s-*\\((\\)"))

(setq sql-oracle-operators-keywords-regexp "\\<\\(all\\|and\\|any\\|between\\|distinct\\|escape\\|exists\\|intersect\\|is\\s-+\\(not\\)?\\s-+null\\|\\(\\(left\\|right\\|full\\|natural\\|cross\\)\\s-+\\)?\\(outer\\s-+\\)?join\\|like\\|minus\\|not\\|null\\|or\\|prior\\|some\\|union\\)\\>")

(setq sql-oracle-plsql-statement-keywords
      (concat "\\b"
              (regexp-opt '(
                            "cursor"
                            "function"
                            "procedure"
                            "type"
                            "record"
                            "trigger"
                            "if"
                            "then"
                            "else"
                            "elsif"
                            "loop"
                            "while"
                            "case"
                            "reverse"
                            "open"
                            "fetch"
                            "close"
                            "exit"
                            "when"
                            "goto"
                            "raise"
                            "return"
                            "declare"
                            "begin"
                            "exception"
                            "index"
                            "of"
                            "immediate"
                            "forall"
                            "using"
                            ) t)
;              "\\s-+"))
              "\\b"))
      
(setq sql-oracle-plsql-statement-keyword-regexp
      (concat "\\b"
              "\\("
              "for\\s-+\\(\\w+\\)\\s-+in"
              "\\|"
              "package\\(\\s-+body\\)?"
              "\\|"
              "is\\s-+table"
              "\\|"
              "end\\(\\s-+\\(if\\|loop\\|case\\)\\)?"
              "\\|"
              "execute\\s-+immediate\\s-+'"
              "\\|"
              "\\(returning\\s-+\\)?bulk\\s-+collect\\s-+into"
              "\\)"
              "\\b"))

(setq sql-oracle-plsql-exceptions
      (concat "\\b\\("
              (regexp-opt '(
                            "access_into_null" "case_not_found" "collection_is_null"
                            "cursor_already_open" "dup_val_on_index" "invalid_cursor"
                            "invalid_number" "login_denied" "no_data_found" "not_logged_on"
                            "others" "program_error" "rowtype_mismatch" "self_is_null"
                            "storage_error" "subscript_beyond_count" "subscript_outside_limit"
                            "sys_invalid_rowid" "timeout_on_resource" "too_many_rows"
                            "value_error" "zero_divide"
                            ) nil)
              "\\|pragma\\s-+\\(exception_init\\|restrict_references\\)"
              "\\)\\b"))

;; FIXME: raw mlslabel; natural
(setq sql-oracle-datatypes
      (concat "\\b"
              "\\(constant\\s-+\\)?"
              "\\("
              (regexp-opt '(
                            ;; SQL datatypes
                            "byte" "bfile" "binary_double" "binary_float" "blob" "char"
                            "clob" "date" "long" "nchar" "nclob" "number" "nvarchar2"
                            "raw" "rowid" "urowid" "varchar2"
                            ;; PLS/SQL datatypes
                            "binary_integer" "boolean" "dec" "decimal"
                            "float" "int" "integer" "naturaln" "numeric"
                            "pls_integer" "positive" "positiven" "real" "signtype"
                            "smallint"
                            ) nil)
              ;; SQL datatypes
              "\\|interval\\(\\s-*(.*)\\)?\\s-+day\\s-+to\\s-+second"
              "\\|interval\\(\\s-*(.*)\\)?\\s-+year\\s-+to\\s-+month"
              "\\|long\\s-+\\(raw\\)?"
              "\\|timestamp\\(\\s-*(.*)\\)?\\(\\s-+with\\(\\s-+local\\)?\\s-+time\\s-+zone\\)?"
              ;; PLS/SQL datatypes
              "\\|double\\s-+precision"
              ;; TABLE OF xxxx
              "\\|table\\s-+of"
              "\\)"
              "\\s-*\\(\\(([0-9,]+)\\|([0-9]+)\\)\\)?"
              "\\b"))

(setq sql-oracle-ansi-datatypes
      (concat "\\b"
              "\\(national\\s-+\\)?\\(n\\|\\(long\\s-+\\)?var\\)?char\\(acter\\)?\\(\\s-+varying\\)?"
              "\\b"))
              
(setq sql-oracle-types
      (concat "\\b"
              (regexp-opt '(
                            ;; AnyData
                            "anydata" "anydataset" "anytype"
                             ;; Oracle Streams AQ TYPEs
                            "aq$_agent" "aq$_agent_list_t" "aq$_descriptor" "aq$_post_info"
                            "aq$_post_info_list" "aq$_purge_options_t" "aq$_recipient_list_t"
                            "aq$_reg_info" "aq$_reg_info_list" "aq$_subscriber_list_t"
                            "dequeue_options_t" "enqueue_options_t" "message_properties_t"
                            "message_properties_array_t" "msgid_array_t"
                            ;; Logical Change Record TYPEs
                            "lcr$_ddl_record" "lcr$_row_record" "lcr$_row_list" "lcr$_row_unit"
                            ;; Rule types
                            "re$attribute_value" "re$attribute_value_list" "re$column_value"
                            "re$column_value_list" "re$name_array" "re$nv_array"
                            "re$nv_list" "re$nv_node" "re$rule_hit" "re$rule_hit_list"
                            "re$table_alias" "re$table_alias_list" "re$table_value"
                            "re$table_value_list" "re$variable_type" "re$variable_type_list"
                            "re$variable_value" "re$variable_value_list"
                            ;; Misc
                            "uritype" "xmltype"
                             ) t)
              "\\b"))

(setq sql-oracle-supplied-plsql-packages
      (concat "\\b"
              (regexp-opt '(
                            "ctx_adm" "ctx_cls" "ctx_ddl" "ctx_doc" "ctx_output"
                            "ctx_query" "ctx_report" "ctx_thes" "ctx_ulexer"
                            "dbms_advanced_rewrite" "dbms_advisor" "dbms_alert"
                            "dbms_application_info" "dbms_apply_adm" "dbms_aq"
                            "dbms_aqadm" "dbms_aqelm" "dbms_capture_adm"
                            "dbms_cdc_publish" "dbms_cdc_subscribe" "dbms_crypto"
                            "dbms_data_mining" "dbms_data_mining_transform"
                            "dbms_datapump" "dbms_ddl" "dbms_debug" "dbms_defer"
                            "dbms_defer_query" "dmbs_defer_sys" "dbms_describe"
                            "dbms_dimension" "dbms_distributed_trust_admin"
                            "dbms_fga" "dmbs_file_transfer" "dmbs_flashback"
                            "dbms_frequent_itemset" "dbms_hs_passthrough" "dbms_iot"
                            "dbms_java" "dbms_job" "dbms_ldap" "dbms_ldap_utl"
                            "dbms_libcache" "dbms_lob" "dbms_lock" "dbms_logmnr"
                            "dbms_logmnr_d" "dbms_logstdby" "dbms_metadata"
                            "dbms_mgwadm" "dbms_mgwmsg" "dbms_monitor" "dbms_mview"
                            "dbms_obfuscation_toolkit" "dbms_odci" "dbms_offline_og"
                            "dbms_olap" "dbms_outln" "dbms_outln_edit" "dbms_output"
                            "dbms_pclxutil" "dbms_pipe" "dbms_profiler"
                            "dbms_propagation_adm" "dbms_random" "dbms_rectifier_diff"
                            "dbms_redefinition" "dbms_refresh" "dbms_repair"
                            "dbms_repcat" "dbms_repcat_admin" "dbms_repcat_instatiate"
                            "dbms_repcat_rgt" "dbms_reputil" "dbms_resource_manager"
                            "dbms_resource_manager_privs" "dbms_resumable" "dbms_rls"
                            "dbms_rowid" "dbms_rule" "dbms_rule_adm" "dbms_scheduler"
                            "dbms_server_alert" "dbms_service" "dbms_session"
                            "dbms_shared_pool" "dbms_space" "dbms_space_admin"
                            "dbms_sql" "dbms_sqltune" "dbms_stat_funcs" "dbms_stats"
                            "dbms_storage_map" "dbms_streams" "dbms_streams_admin"
                            "dbms_streams_auth" "dbms_streams_messaging"
                            "dbms_streams_tablespace_adm" "dbms_trace" "dbms_transaction"
                            "dbms_transform" "dbms_tts" "dbms_types" "dbms_utility"
                            "dbms_warning" "dbms_workload_repository" "dbms_wm"
                            "dbms_xdb" "dbms_xdb_version" "dbms_xdbt" "dbms_xdbz"
                            "dbms_xmlgen" "dbms_xmldom" "dbms_xmlparser" "dmbs_xmlquery"
                            "dbms_xmlsave" "dbms_xmlschema" "dbms_xmlstore" "dbms_xplan"
                            "dbms_xslprocessor" "debug_extproc" "htf" "htmldb_application"
                            "htmldb_custom_auth" "htmldb_item" "htmldb_util" "htp"
                            "owa_cache" "owa_cookie" "owa_custom" "owa_image"
                            "owa_opt_lock" "owa_pattern" "owa_sec" "owa_text" "owa_util"
                            "sdo_cs" "sdo_gcdr" "sdo_geom" "sdo_geor" "sdo_geor_utl"
                            "sdo_lrs" "sdo_migrate" "sdo_net" "sdo_sam" "sdo_topo"
                            "sdo_topo_map" "sdo_tune" "sdo_util" "utl_coll" "utl_compress"
                            "utl_dbws" "utl_encode" "utl_file" "utl_http" "utl_i18n"
                            "utl_inaddr" "utl_lms" "utl_mail" "utl_raw" "utl_recomp"
                            "utl_ref" "utl_smtp" "utl_tcp" "utl_url" "wpg_docload"
                            ) t)
              "\\b"))

;; FIXME: start
(setq sqlplus-oracle-commands
      (concat "^\\s-*"
              "\\("
              (regexp-opt '(
                            "append" "attribute" "btitle" "change" "clear"
                            "compute" "copy" "define" "del" "describe"
                            "disconnect" "edit" "get" "help" "host" "input"
                            "list" "password" "pause" "print" "quit"
                            "repfooter" "repheader" "run" "save"
                            "spool" "store" "timing" "ttitle" "undefine"
                            ) nil)
              "\\|"
              "archive\\s-+log\\s-+\\(list\\|start\\|stop\\)"
              "\\|"
              "recover\\s-+\\(data\\(base\\|file\\)\\|tablespace\\)"
              "\\|"
              "show?\\s-+\\(parameters?\\|all\\)?"
              "\\|"
              "shutdown\\s-+\\(normal\\|immediate\\|abort\\)?"
              "\\|"
              "startup\\s-+\\(\\(no\\)?mount\\|open\\|force\\|pfile\\)?"
              "\\|"
              "whenever\\s-+\\(os\\|sql\\)error"
              "\\)"
             "\\s-+"
              ))

(setq sqlplus-oracle-command-keyword-regexp
      (concat "\\b"
              "\\("
              "until\\s-+\\(cancel\\|time\\)"
              "\\|"
              "using\\s-+backup\\s-+controlfile"
              "\\)"
              "\\s-+"
              ))

(setq sql-oracle-font-lock-keywords
      (list
       ;; Aliasnames
       ;;       '("\\s-+from\\(\\s-+[a-z]+[a-z0-9$#_]*\\s-+\\([a-z]+[a-z0-9$#_]*\\)?,?\\)+" 2 sqlplus-type-face nil t)
       '("\\([a-z]+[a-z0-9$#_]*\\s-*\\.\\s-*\\)+" 0 sqlplus-type-face)
       ;; semicolon ";" at end of statement
       '(";" . sqlplus-semicolon-face)
       ;; "/" at beginning of line to send statement to sqlplus
       '("^/" . sqlplus-send-to-interpreter-face)
       ;; SQL functions
       ;;       (list (concat sql-oracle-functions-keywords-regexp) '(1 sqlplus-function-name-face t) '(6 sqlplus-function-name-face t))
       (list (concat sql-oracle-functions-keywords-regexp ) '(1 sqlplus-builtin-face t))
       ;; Special Functions
       '("\\s-*\\<\\(sysdate\\|uid\\|user\\)\\>\\s-*" 0 sqlplus-special-functions-face t)
       ;; FIXME
;       '("\\s-*\\<\\(fixme\\)\\>\\s-*>" 0 sql-oracle-fixme-face t)
       ;; Pseudo-Columns
       '("\\s-*\\<\\(connect_by_is\\(cycle\\|leaf\\)\\|\\(column\\|object\\)_value\\|\\(curr\\|next\\)val\\|level\\|row\\(id\\|label\\|num\\)\\)\\>\\s-*" 0 sqlplus-pseudo-columns-face t)
       ;; Operators
       (list (concat sql-oracle-operators-keywords-regexp) '(0 sqlplus-keyword-face))
       '("\\((\\+)\\|||\\)" . sqlplus-keyword-face)
       ;; PL/SQL statement keyword regexp
       (list (concat sql-oracle-plsql-statement-keyword-regexp) '(0 sqlplus-keyword-face) '(2 default t t))
       ;; PL/SQL statement keywords
       (list (concat sql-oracle-plsql-statement-keywords) '(0 sqlplus-keyword-face))
       ;; SQL statement keyword regexp
       (list (concat sql-oracle-sql-statement-keyword-regexp) '(0 sqlplus-keyword-face))
       ;; SQL statement keywords
       (list (concat sql-oracle-sql-statement-keywords) '(0 sqlplus-keyword-face))
       ;; SQL statements
       (list (concat sql-oracle-sql-statements) '(0 sqlplus-keyword-face))
       ;; delete
					;       (list (concat "^\\s-*\\(delete\\s-+\\(from\\s-+\\)?\\)\\(\\w+\\)\\>") '(1 sqlplus-keyword-face t) '(2 default t t))
					;             '("\\(\\s-+\\(and\\|or\\|in\\|where\\|returning\\|subpartition\\|partition\\|with\\|read\\|only\\|check\\|option\\|constraint\\|into\\)\\)\\>"
					;               (re-search-forward "^$") nil (0 sqlplus-keyword-face t)))
       ;; (list (concat "^\\s-*delete\\s-+\\(from\\s-+\\)?\\(\\w+\\)\\>") '(0 sqlplus-keyword-face t) '(2 default t t)
       ;;       '("\\(\\s-+\\(and\\|or\\|in\\|where\\|returning\\|subpartition\\|partition\\|with\\|read\\|only\\|check\\|option\\|constraint\\|into\\)\\)\\>"
       ;;         (re-search-forward "^$") nil (0 sqlplus-keyword-face t)))
       ;; SQL-DDL commands
       (list (concat sql-oracle-ddl-keywords-regexp) '(0 sqlplus-keyword-face t t))
       ;; SQL*Plus commands
       (list (concat sqlplus-oracle-command-keyword-regexp) '(0 sqlplus-reference-face t))
       ;; SQL*Plus command keyword regexp
       (list (concat sqlplus-oracle-commands) '(0 sqlplus-reference-face t))
       ;; SQL*Plus commands
       (list (concat sqlplus-oracle-commands) '(0 sqlplus-reference-face t))
       '("@@?" . sqlplus-reference-face)
       ;; Predefined PL/SQL exceptions
       (list (concat sql-oracle-plsql-exceptions) '(0 sqlplus-warning-face))
       ;; SQL ANSI datatypes
       (list (concat sql-oracle-ansi-datatypes) '(0 sqlplus-variable-name-face t))
       ;; Oracle SQL and PL/SQL datatypes
       (list (concat sql-oracle-datatypes) '(0 sqlplus-variable-name-face t))
       '("%\\(row\\)?type" . sqlplus-variable-name-face)
       ;; Oracle types
       (list (concat sql-oracle-types) '(0 sqlplus-preprocessor-face t))
       ;; Oracle supplied PL/SQL packages
       (list (concat sql-oracle-supplied-plsql-packages) '(0 sqlplus-preprocessor-face t))
       ;;
       ;; ACC[EPT] variable [NUM[BER] | CHAR | DATE | BINARY_FLOAT | BINARY_DOUBLE] [FOR[MAT] format] [DEF[AULT] default] [PROMPT text|NOPR[OMPT]] [HIDE]
       ;;
       ;; (list (concat "^\\s-*acc\\(ept\\)?\\s-+\\(\\w+\\s-+\\)\\(num\\(ber\\)?\\|char\\|date\\|binary_float\\|binary_double\\)?\\>") '(0 sqlplus-reference-face t) '(2 default t t)
       ;; 	     '("\\s-+\\(for\\(mat\\)?\\|def\\(ault\\)?\\|\\(no\\)?pr\\(ompt\\)?\\|hide\\)\\>"
       ;; 	       nil nil (1 sqlplus-reference-face t)))

       (list (concat "^\\s-*\\(acc\\(ept\\)?\\)\\s-+\\(\\w+\\s-+\\)\\(num\\(ber\\)?\\|char\\|date\\|binary_float\\|binary_double\\)?\\>") '(1 sqlplus-reference-face t) '(4 sqlplus-reference-face t)
	     '("\\s-+\\(for\\(mat\\)?\\|def\\(ault\\)?\\|\\(no\\)?pr\\(ompt\\)?\\|hide\\)\\>"
	       nil nil (1 sqlplus-reference-face t)))

       ;;
       ;; BRE[AK] [ON report_element [action [action]]] ...
       ;; where report_element has the syntax {column|expr|ROW|REPORT}
       ;; and action has the syntax [SKI[P] n|[SKI[P]] PAGE] [NODUP[LICATES]|DUP[LICATES]]
       ;;
       (list (concat "^\\s-*bre\\(ak\\)?\\>") '(0 sqlplus-reference-face t)
	     '("\\s-+\\(on\\(\\s-+r\\(ow\\|eport\\)\\)?\\|skip?\\(\\s-+page\\)?\\|\\(no\\)?dup\\(licates\\)?\\)\\>"
	       nil nil (1 sqlplus-reference-face t)))
       ;;

       ;; COL[UMN] [{column | expr} [option ...]]
					;where option represents one of the following clauses:
					;ALI[AS] alias
					;CLE[AR]
					;ENTMAP {ON | OFF}
					;FOLD_A[FTER]
					;FOLD_B[EFORE]
					;FOR[MAT] format
					;HEA[DING] text
					;JUS[TIFY] {L[EFT] | C[ENTER] | R[IGHT]}
					;LIKE {expr | alias}
					;NEWL[INE]
					;NEW_V[ALUE] variable
					;NOPRI[NT] | PRI[NT]
					;NUL[L] text
					;OLD_V[ALUE] variable
					;ON | OFF
					;WRA[PPED] | WOR[D_WRAPPED] | TRU[NCATED]
       ;; (list (concat "^\\s-*col\\(umn\\)?\\s-+\"?\\(\\w+\\)\"?\\>") '(0 sqlplus-reference-face t) '(2 default t t)
       ;; 					;(regexp-opt '(
       ;; 					;              "ali" "alias" "c" "center" "cle" "clear" "entmap"
       ;; 					;              "fold_a" "fold_after" "fold_b" "fold_before" "for"
       ;; 					;              "format" "hea" "heading" "jus" "justify" "l" "left"
       ;; 					;              "like" "new_v" "new_value" "newl" "newline" "nopri"
       ;; 					;              "noprint" "nul" "null" "old_v" "old_value" "on" "off"
       ;; 					;              "pri" "print" "r" "right" "tru" "truncated" "wor"
       ;; 					;              "word_wrapped" "wra" "wrapped"                             
       ;; 					;              ) t)"
       ;; 	     '("\\s-+\\(ali\\(?:as\\)?\\|c\\(?:enter\\|le\\(?:ar\\)?\\)\\|entmap\\|fo\\(?:ld_\\(?:after\\|before\\|[ab]\\)\\|r\\(?:mat\\)?\\)\\|hea\\(?:ding\\)?\\|jus\\(?:tify\\)?\\|l\\(?:eft\\|ike\\)\\|n\\(?:ew\\(?:_v\\(?:alue\\)?\\|l\\(?:ine\\)?\\)\\|opri\\(?:nt\\)?\\|ull?\\)\\|o\\(?:ff\\|ld_v\\(?:alue\\)?\\|n\\)\\|pri\\(?:nt\\)?\\|right\\|tru\\(?:ncated\\)?\\|w\\(?:or\\(?:d_wrapped\\)?\\|ra\\(?:pped\\)?\\)\\|[clr]\\)\\>"
       ;; 	       nil nil (1 sqlplus-reference-face t)))

       (list (concat "^\\s-*\\(col\\(umn\\)?\\)\\s-+\\(\"?\\w+\"?\\)") '(1 sqlplus-reference-face t)
	     '("\\s-+\\(ali\\(?:as\\)?\\|c\\(?:enter\\|le\\(?:ar\\)?\\)\\|entmap\\|fo\\(?:ld_\\(?:after\\|before\\|[ab]\\)\\|r\\(?:mat\\)?\\)\\|hea\\(?:ding\\)?\\|jus\\(?:tify\\)?\\|l\\(?:eft\\|ike\\)\\|n\\(?:ew\\(?:_v\\(?:alue\\)?\\|l\\(?:ine\\)?\\)\\|opri\\(?:nt\\)?\\|ull?\\)\\|o\\(?:ff\\|ld_v\\(?:alue\\)?\\|n\\)\\|pri\\(?:nt\\)?\\|right\\|tru\\(?:ncated\\)?\\|w\\(?:or\\(?:d_wrapped\\)?\\|ra\\(?:pped\\)?\\)\\|[clr]\\)\\>"
	       nil nil (1 sqlplus-reference-face t)))
       
       ;; CONN[ECT] { logon | / } [AS {SYSOPER | SYSDBA}]
       (list (concat "^\\s-*\\(conn\\(ect\\)?\\s-+\\(as\\s-+-sys\\(dba\\|oper\\)\\|internal\\)?\\)") '(0 sqlplus-reference-face))
       ;; {EXIT | QUIT} [SUCCESS | FAILURE | WARNING | n | variable | :BindVariable] [COMMIT | ROLLBACK]
       (list (concat "^\\s-*\\(\\(exit\\|quit\\)\\s-+$\\)") '(0 sqlplus-reference-face t))
       ;; EXEC[UTE] statement
       (list (concat "^\\s-*\\(\\(exec\\(ute\\)?\\)\\s-+" sql-oracle-schema-object-name "\\((.*)\\)?" sqlplus-oracle-send-command-regexp "\\)") '(2 sqlplus-reference-face t))
       ;; PRO[MPT] [text]
       (list (concat "^\\s-*\\(\\(pro\\(mpt\\)?\\s-+\\)\\(.*\\)?$\\)") '(0 sqlplus-reference-face t) '(4 sqlplus-string-face t t))
       ;; SET xxx
       (list (concat "^\\s-*set\\s-+\\(\\(appi\\(nfo\\)?\\|array\\(size\\)?\\|auto\\(commit\\)?\\|autop\\(rint\\)?\\|autorecovery\\|autot\\(race\\)?\\|blo\\(ckterminator\\)?\\|cmds\\(ep\\)?\\|colsep\\|com\\(patibility\\)?\\|con\\(cat\\)?\\|copyc\\(ommit\\)?\\|copytypecheck\\|def\\(ine\\)?\\|describe\\|echo\\|editf\\(ile\\)?\\|emb\\(edded\\)?\\|esc\\(ape\\)?\\|feed\\(back\\)?\\|flagger\\|flu\\(sh\\)?\\|hea\\(ding\\)?\\|heads\\(ep\\)?\\|instance\\|lin\\(esize\\)?\\|lobof\\(fset\\)?\\|logsource\\|longc\\(hunksize\\)?\\|mark\\(up\\)?\\|newp\\(age\\)?\\|null\\|numf\\(ormat\\)?\\|pages\\(ize\\)?\\|pau\\(se\\)?\\|recsep\\|recsepchar\\|serverout\\(put\\)?\\|shift\\(inout\\)?\\|show\\(mode\\)?\\|sqlbl\\(anklines\\)?\\|sqlc\\(ase\\)?\\|sqlco\\(ntinue\\)?\\|sqln\\(umber\\)?\\|sqlpluscompat\\(ibility\\)?\\|sqlpre\\(fix\\)?\\|sqlp\\(rompt\\)?\\|sqlt\\(erminator\\)?\\|suf\\(fix\\)?\\|tab\\|term\\(out\\)?\\|ti\\(me\\)?\\|timi\\(ng\\)?\\|trim\\(out\\)?\\|trims\\(pool\\)?\\|und\\(erline\\)?\\|ver\\(ify\\)?\\|wra\\(p\\)?\\)\\s-+\\)\\(on\\|off\\)?") '(0 sqlplus-reference-face t))
       ;; VAR[IABLE] [variable [NUMBER | CHAR | CHAR (n [CHAR | BYTE]) | NCHAR | NCHAR (n) | VARCHAR2 (n [CHAR | BYTE]) | NVARCHAR2 (n) | CLOB | NCLOB | REFCURSOR | BINARY_FLOAT | BINARY_DOUBLE] ]
       (list (concat "^\\s-*\\(\\(var\\(iable\\)?\\s-+\\)\\(\\(" sql-oracle-nonquoted-identifier "\\s-+\\)\\(number\\|n?char\\(([0-9]+)\\)?\\|n?varchar2\\(([0-9]+)\\)\\|n?clob\\|refcursor\\|binary_\\(float\\|double\\)\\)\\s-+\\)?\\)") '(0 sqlplus-reference-face t) '(6 sqlplus-variable-name-face t t) '(7 sqlplus-type-face t t) '(8 default t t) '(9 default t t))

       ;; These should be rehighlighted because of duplicate meanings
       '("^\\s-*\\(update\\)\\s-+\\w+[ \t\n]+\\(set\\)\\b" '(1 sqlplus-keyword-face t) '(2 sqlplus-keyword-face t))
;       '("\\s-*\\b\\(natural\\s-+join\\)\\b" '(1 sqlplus-keyword-face t))

       ;; FIXMEs
;       '("^FIXME\\b.*$" 0 sql-oracle-fixme-face t)
       
       ;; Strings
       '("\\('[^'\n]*'\\|\"[^\"\n]*\"\\)" 0 sqlplus-string-face t)
       
       ;; Comments
       '("--.*$" 0 sqlplus-comment-face t)
       '("^rem\\(ark\\)?\\b.*$" 0 sqlplus-comment-face t)
       '("/\\*.*/" 0 sqlplus-comment-face t)
       
;;        (list "'\\([^']*\\)'" 1 'font-lock-string-face t)
;;        (list "\\(--.*\\)" 1 'sqlplus-comment-face t)
;;        (list "\\(/\\*.*\\*/\\)" 1 'sqlplus-comment-face t)
;;        (list "^\\(rem\\([ \t].*\\)?\\)$" 1 'sqlplus-comment-face t)
;; ;       (list "^\\(prompt\\([ \t].*\\)?\\)$" 1 'sqlplus-comment-face t)

       "Additional expressions to highlight in SQL mode."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sql-oracle-keywords-regexp "\\<\\(select\\|into\\|from\\|where\\(\\s-+current\\s-+off\\)?\\|\\(group\\|order\\|connect\\)\\s-+by\\|alter\\|commit\\|controlfile\\s-+\\(reuse\\)?\\|datafile\\|logfile\\|group\\|size\\|tablespace\\|temporary\\|delete\\|drop\\|grant\\|insert\\(\\s-+into\\)?\\|values\\|rename\\|revoke\\|rollback\\|savepoint\\|start\\s-+with\\|update\\|having\\|as\\|is\\|in\\|out\\|\\(primary\\|foreign\\)\\s-+key\\|unique\\|constraint\\|default\\|references\\|check\\|on\\s-+delete\\s-+cascade\\|asc\\|desc\\|add\\|modify\\|enable\\|disable\\|o\\(n\\|ff\\)line\\|reuse\\|read\\s-+\\(only\\|write\\)\\|to\\|from\\|on\\|set\\|identified\\s-+by\\|switch\\|cascade\\|\\(no\\)?archivelog\\|for\\s-+update\\|comment\\s-+on\\s-+\\(table\\|column\\)\\)\\>")
(setq plsql-oracle-keywords-regexp "\\<\\(cursor\\|function\\|package\\(\\s-+body\\)?\\|procedure\\|type\\|table\\|record\\|trigger\\|if\\|then\\|else\\|elsif\\|end\\s-+\\(if\\|loop\\)\\|loop\\|while\\|for\\|open\\|fetch\\|close\\|exit\\|when\\|goto\\|raise\\|return\\|declare\\|begin\\|exception\\|end\\|index\\|by\\|of\\)\\>")
(setq sqlplus-oracle-keywords-regexp "\\<\\(accept\\|append\\|break\\|btitle\\|change\\|clear\\|column\\(\\s-+\\(clear\\|default\\)\\)?\\|compute\\|connect\\|copy\\|define\\|del\\|describe\\|disconnect\\document\\|edit\\|execute\\|get\\|help\\|host\\|input\\|list\\|newpage\\|pause\\|print\\|prompt\\|quit\\|run\\(form\\)?\\|save\\|set\\s-+\\(feed\\(back\\)?\\|numwidth\\|head\\(ing\\)?\\|verify\\|term\\(out\\)?\\|echo\\)\\|show\\(\\s-+all\\)?\\|spool\\|sqlplus\\|start\\|timing\\|ttitle\\|undefine\\|variable\\|whenever\\s-+\\(os\\|sql\\)error\\)\\>")
(setq plsql-oracle-datatypes-keywords-regexp "\\<\\(cursor_already_open\\|dup_val_on_index\\|invalid_\\(cursor\\|number\\)\\|login_denied\\|no_data_found\\|not_logged_on\\|\\(program\\|storage\\|value\\)_error\\|timeout_on_resource\\|too_many_rows\\|transaction_backed_out\\|zero_divide\\|others\\|pragma\\s-+\\(exception_init\\|restrict_references\\)\\|\\(constant\\s-+\\)?\\(binary_integer\\|boolean\\|char\\(acter\\)?\\|date\\|dec\\(imal\\)?\\|double\\s-+precision\\|float\\|int\\(eger\\)?\\|\\long\\s-+\\(raw\\)?\\|mlslabel\\|natural\\|num\\(ber\\|eric\\)\\|positive\\|raw\\(\\s-+mlslabel\\)?\\|real\\|smallint\\|string\\|varchar2?\\)\\s-*\\(\\(([0-9,]+)\\|([0-9]+)\\)\\)?\\)\\>")

(setq sqlplus-oracle-font-lock-keywords 
      (list 
       ;; Aliasnames
       (list 
	(concat sqlplus-oracle-prompt-regexp)
	'(0 sqlplus-type-face) 
	(list 
	 (concat "\\([a-z]+[a-z0-9$#_]*\\s-*\\.\\s-*\\)+") nil nil '(0 sqlplus-type-face t)))
       ;; / and ;
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat "\\(;\\|/\\)") nil nil '(1 sqlplus-variable-name-face)))
       ;; SQL*Plus commands
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat sqlplus-oracle-keywords-regexp) nil nil '(1 sqlplus-reference-face)))
       '("@@?" . sqlplus-reference-face)
       ;; SQL keywords
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat sql-oracle-keywords-regexp) nil nil '(1 sqlplus-variable-name-face t)))
       ;; SQL-DDL commands
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat sql-oracle-ddl-keywords-regexp) nil nil '(1 sqlplus-variable-name-face t)))
       ;; SQL functions
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat sql-oracle-functions-keywords-regexp) nil nil '(1 sqlplus-function-name-face t) '(6 sqlplus-function-name-face t)))
       ;; Special Functions
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat "\\s-*\\<\\(sysdate\\|uid\\|user\\)\\>\\s-*") nil nil '(1 sqlplus-function-name-face t)))
       ;; Pseudo-Columns
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat "\\<\\(\\(curr\\|next\\)val\\|level\\|row\\(id\\|label\\|num\\)\\)\\>\\s-*") nil nil '(1 sqlplus-function-name-face t)))
       ;; Operators
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat sql-oracle-operators-keywords-regexp) nil nil '(1 sqlplus-variable-name-face t)))
       '("\\((\\+)\\|||\\)" . sqlplus-variable-name-face)
       ;; PL/SQL key words
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat plsql-oracle-keywords-regexp) nil nil '(1 sqlplus-variable-name-face t)))
       ;; Datatypes
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat plsql-oracle-datatypes-keywords-regexp) nil nil '(0 sqlplus-type-face t)))
       (list 
	(concat sqlplus-oracle-prompt-regexp) 
	'(0 sqlplus-type-face) 
	(list 
	 (concat "%\\(row\\)?type") nil nil '(0 sqlplus-type-face t)))
       ;; Error Messages
       '("^\\(ERROR at line \\|unknown command\\|PL/SQL: \\).*$" 0 sqlplus-type-face)
       '("^\\([A-Z][A-Z][A-Z]-[0-9][0-9][0-9][0-9][0-9]:\\) .*$" 1 sqlplus-type-face)
       ;; xx row[s]/Statement selected/processed (SQL*PLus/ServerManager)
       '("^\\([0-9]* rows?\\|Statement\\) \\(selected\\|processed\\)\." . sqlplus-type-face)
       ;; Strings
       '("\\('[^'\n]*'\\|\"[^\"\n]*\"\\)" 0 sqlplus-string-face t)
       ;; Comments
       '("\\(\\<rem\\(ark\\)?\\>\\s-+\\|--\\).*$" 0 sqlplus-comment-face t)
       '("/\\*.*/" 0 sqlplus-comment-face t)
       ;; "Output from buffer" message starting command output
       (list (concat sqlplus-output-message ".*$") 0 sqlplus-output-from-buffer-face t)
       ;; SQL*Plus-mode banner
       '("^#.*#" 0 sqlplus-comment-face t)
       "Additional expressions to highlight in SQL*Plus mode."))

(setq plsql-oracle-font-lock-keywords
      (list
       ;; Aliasnames
;       '("\\s-+from\\(\\s-+[a-z]+[a-z0-9$#_]*\\s-+\\([a-z]+[a-z0-9$#_]*\\)?,?\\)+" 2 sqlplus-type-face nil t)
       '("\\([a-z]+[a-z0-9$#_]*\\s-*\\.\\s-*\\)+" 0 sqlplus-type-face)
       ;; / and ;
       '("\\(;\\|/\\)" . sqlplus-variable-name-face)
       ;; SQL*Plus commands
       (list (concat sqlplus-oracle-keywords-regexp) '(1 sqlplus-reference-face))
       '("@@?" . sqlplus-reference-face)
       ;; SQL keywords
       (list (concat sql-oracle-keywords-regexp) '(1 sqlplus-variable-name-face t))
       ;; SQL-DDL commands
       (list (concat sql-oracle-ddl-keywords-regexp) '(0 sqlplus-variable-name-face t t))
       ;; SQL functions
       (list (concat sql-oracle-functions-keywords-regexp) '(1 sqlplus-function-name-face t) '(6 sqlplus-function-name-face t))
       ;; Special Functions
       '("\\s-*\\<\\(sysdate\\|uid\\|user\\)\\>\\s-*" 0 sqlplus-function-name-face t)
       ;; Pseudo-Columns
       '("\\<\\(\\(curr\\|next\\)val\\|level\\|row\\(id\\|label\\|num\\)\\)\\>\\s-*" . sqlplus-function-name-face)
       ;; Operators
       (list (concat sql-oracle-operators-keywords-regexp) '(0 sqlplus-variable-name-face))
       '("\\((\\+)\\|||\\)" . sqlplus-variable-name-face)
       ;; PL/SQL key words
       (list (concat plsql-oracle-keywords-regexp) '(0 sqlplus-variable-name-face))
       ;; Datatypes
       (list (concat plsql-oracle-datatypes-keywords-regexp) '(0 sqlplus-type-face))
       '("%\\(row\\)?type" . sqlplus-type-face)
       ;; Strings
       '("\\('[^'\n]*'\\|\"[^\"\n]*\"\\)" 0 sqlplus-string-face t)
       ;; Comments
       '("\\(\\<rem\\(ark\\)?\\>\\s-+\\|--\\).*$" 0 sqlplus-comment-face t)
       '("/\\*.*/" 0 sqlplus-comment-face t)
       "Additional expressions to highlight in PL/SQL mode."))

(provide 'sql-oracle-fontlock)

;      ;; Startup messages for SQL*Plus and Server Manager
;      ("^Connected to:$" nil purple)
;      ("^Oracle[0-9] Server Release [0-9]\\.[0-9]\\.[0-9]\\.[0-9]\\.[0-9] - .* Release$" nil purple)
;      ("^With the [a-zA-Z, ]* options$" nil purple)
;      ("^PL/SQL Release [0-9]\\.[0-9]\\.[0-9]\\.[0-9]\\.[0-9] - Production$" nil purple)
;      ("^SQL\\*Plus: Release [0-9]\\.[0-9]\\.[0-9]\\.[0-9]\\.[0-9] - [a-zA-Z0-9: ]*$" nil purple)
;      ("^Oracle Server Manager Release [0-9]\\.[0-9]\\.[0-9]\\.[0-9]\\.[0-9] - .*$" nil purple)
;      ("^Copyright (c) [0-9a-zA-Z,\. ]*\\.$" nil purple)


;      ;; Operators
;      ("\\<in[ \t]*\n?[ \t]*(" nil Navy)

;      ("\\(declare\\|begin\\|exception\\>\\|end[ \t\n]*\\([a-z0-9$#_]+\\)?[ \t\n]*;\\)[ \t\n]*" nil label)

;      ;; PL/SQL Structures
;      ("\\<\\(\\)\\>" nil ForestGreen-bold)

;      ;; PL/SQL Exception key words
;      ("\\<\\>" nil lightseagreen)

;      ;; Oracle V7 Packages for use in PL/SQL
;      ("dbms_[a-z0-9$#_]+\\.[a-z0-9$#_]+[ \t\n]*(" nil purple-bold)

;;;; Running SQL commands

;      ;; \ at the beginning in his own line
;      ("^/[ \t]*$" nil secondary-selection)

;      ;; Substitution variables and parameters (Austauschvariablen) in SQL*Plus
;      ;; Variables in PL/SQL
;      ("\\(&&?\\|:\\)[a-z]+[a-z0-9$#_]*[^a-z0-9$#_]" nil darkcyan)

;      ;; Labels in PL/SQL
;      ("<<[a-z]+[a-z0-9$#_]*>>" nil darkgreen-bold)

;      ;; Cursor Attributes in PL/SQL (implizit and explicit)
;      ("\\(sql\\)?%\\(\\(not\\)?found\\|rowcount\\|isopen\\)" nil pink)

;      ;; Assignments and .. in FOR-Loops in PL/SQL
;      ("\\(:=\\|\\.\\.\\)" nil Navy)

;      ;; Predefined exceptions in PL/SQL
;      ("\\<\\(\\)\\>" nil lightseagreen)

;      ;; Aliasnames for tables, Structure Variables




;      ;; Functions
;      ("sql\\(code\\|errm\\)" nil goldenrod)

;      ))
;   nil 'case-insensitive))

;; End of sql-oracle-fontlock.el
