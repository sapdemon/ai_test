CLASS zbp_i_request DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zi_request.
ENDCLASS.

CLASS zbp_i_request IMPLEMENTATION.
ENDCLASS.

CLASS lhc_request DEFINITION INHERITING FROM cl_abap_behavior_handler.
	PROTECTED SECTION.
		METHODS SetRequestNumberAndCreated FOR DETERMINE ON SAVE
			IMPORTING keys FOR Request.
ENDCLASS.

CLASS lhc_request IMPLEMENTATION.
	METHOD SetRequestNumberAndCreated.
		DATA lt_req TYPE TABLE OF zi_request.
		READ ENTITIES OF zi_request IN LOCAL MODE
			ENTITY Request
			ALL FIELDS WITH CORRESPONDING #( keys )
			RESULT DATA(lt_hdr).

		IF lt_hdr IS INITIAL.
			RETURN.
		ENDIF.

		DATA lv_max TYPE zreq_hdr-req_id.
		SELECT MAX( req_id ) INTO @lv_max FROM zreq_hdr WHERE client = @sy-mandt.
		IF sy-subrc <> 0 OR lv_max IS INITIAL.
			lv_max = 0.
		ENDIF.

		DATA lt_upd TYPE TABLE FOR UPDATE zi_request\Request.
		LOOP AT lt_hdr ASSIGNING FIELD-SYMBOL(<hdr>).
			lv_max = lv_max + 1.
			APPEND VALUE #( RequestUUID = <hdr>-RequestUUID
				CreatedAt = cl_abap_context_info=>get_system_time( )
				RequestID = lv_max ) TO lt_upd.
		ENDLOOP.

		MODIFY ENTITIES OF zi_request IN LOCAL MODE
			ENTITY Request UPDATE FIELDS ( CreatedAt RequestID ) WITH lt_upd
			FAILED DATA(failed) REPORTED DATA(reported).
	ENDMETHOD.
ENDCLASS.

CLASS lhc_item DEFINITION INHERITING FROM cl_abap_behavior_handler.
	PROTECTED SECTION.
		METHODS SetItemDefaults FOR DETERMINE ON SAVE
			IMPORTING keys FOR Item.
ENDCLASS.

CLASS lhc_item IMPLEMENTATION.
	METHOD SetItemDefaults.
		READ ENTITIES OF zi_request IN LOCAL MODE
			ENTITY Item
			ALL FIELDS WITH CORRESPONDING #( keys )
			RESULT DATA(lt_items).

		IF lt_items IS INITIAL.
			RETURN.
		ENDIF.

		DATA lt_grouped TYPE SORTED TABLE OF zreq_item WITH UNIQUE KEY parent_uuid.
		DATA lt_upd TYPE TABLE FOR UPDATE zi_request\Item.

		" Pre-fetch current max PositionNo per parent
		DATA lt_parents TYPE SORTED TABLE OF sysuuid_x16 WITH UNIQUE KEY table_line.
		LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<i>).
			INSERT <i>-RequestUUID INTO TABLE lt_parents.
		ENDLOOP.

		DATA ls_parent TYPE sysuuid_x16.
		LOOP AT lt_parents INTO ls_parent.
			DATA(lv_curr_max) = 0.
			SELECT MAX( position_no ) INTO @lv_curr_max FROM zreq_item
				WHERE client = @sy-mandt AND parent_uuid = @ls_parent.
			IF sy-subrc <> 0 OR lv_curr_max IS INITIAL.
				lv_curr_max = 0.
			ENDIF.
			" Assign numbers to created items for this parent
			LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ci>) WHERE RequestUUID = ls_parent.
				lv_curr_max = lv_curr_max + 1.
				APPEND VALUE #( ItemUUID    = <ci>-ItemUUID
					PositionNo  = lv_curr_max
					Currency    = COND #( WHEN <ci>-Currency IS INITIAL THEN 'UAH' ELSE <ci>-Currency ) ) TO lt_upd.
			ENDLOOP.
		ENDLOOP.

		IF lt_upd IS NOT INITIAL.
			MODIFY ENTITIES OF zi_request IN LOCAL MODE
				ENTITY Item UPDATE FIELDS ( PositionNo Currency ) WITH lt_upd
				FAILED DATA(failed) REPORTED DATA(reported).
		ENDIF.
	ENDMETHOD.
ENDCLASS.