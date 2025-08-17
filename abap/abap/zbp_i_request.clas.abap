CLASS zbp_i_request DEFINITION PUBLIC FINAL CREATE PUBLIC.
	PUBLIC SECTION.
		INTERFACES if_oo_adt_classrun.
		INTERFACES if_abap_behavior_handler.
	PROTECTED SECTION.
	PRIVATE SECTION.
ENDCLASS.

CLASS zbp_i_request IMPLEMENTATION.
	METHOD if_oo_adt_classrun~main.
		out->write( |Behavior Pool for ZI_Request| ).
	ENDMETHOD.

	METHOD if_abap_behavior_handler~create.
		DATA: lt_hdr TYPE TABLE OF zreq_hdr,
			lt_itm TYPE TABLE OF zreq_item.

		LOOP AT entities ASSIGNING FIELD-SYMBOL(<e>).
			IF <e>-entity = 'REQUEST'.
				DATA(ls_hdr) = VALUE zreq_hdr( client = sy-mandt req_uuid = cl_system_uuid=>create_uuid_x16( ) ).
				ls_hdr-created_at = cl_abap_context_info=>get_system_time( ).
				SELECT MAX( req_id ) INTO @ls_hdr-req_id FROM zreq_hdr WHERE client = @sy-mandt.
				ls_hdr-req_id = COALESCE( ls_hdr-req_id, 0 ) + 1.
				ls_hdr-description = <e>-data-description.
				APPEND ls_hdr TO lt_hdr.
			ELSEIF <e>-entity = 'ITEM'.
				DATA(ls_itm) = VALUE zreq_item( client = sy-mandt item_uuid = cl_system_uuid=>create_uuid_x16( ) ).
				ls_itm-parent_uuid = <e>-data-requestuuid.
				SELECT MAX( position_no ) INTO @ls_itm-position_no FROM zreq_item WHERE client = @sy-mandt AND parent_uuid = @ls_itm-parent_uuid.
				ls_itm-position_no = COALESCE( ls_itm-position_no, 0 ) + 1.
				ls_itm-amount = <e>-data-amount.
				ls_itm-currency = COND #( WHEN <e>-data-currency IS INITIAL THEN 'UAH' ELSE <e>-data-currency ).
				ls_itm-description = <e>-data-description.
				ls_itm-department_id = <e>-data-departmentid.
				APPEND ls_itm TO lt_itm.
			ENDIF.
		ENDLOOP.

		IF lt_hdr IS NOT INITIAL.
			INSERT zreq_hdr FROM TABLE @lt_hdr.
		ENDIF.
		IF lt_itm IS NOT INITIAL.
			INSERT zreq_item FROM TABLE @lt_itm.
		ENDIF.
	ENDMETHOD.

	METHOD if_abap_behavior_handler~update.
		LOOP AT entities ASSIGNING FIELD-SYMBOL(<e>).
			IF <e>-entity = 'REQUEST'.
				UPDATE zreq_hdr SET description = @<e>-data-description WHERE client = @sy-mandt AND req_uuid = @<e>-key-requestuuid.
			ELSEIF <e>-entity = 'ITEM'.
				UPDATE zreq_item SET amount = @<e>-data-amount, currency = @<e>-data-currency, description = @<e>-data-description, department_id = @<e>-data-departmentid WHERE client = @sy-mandt AND item_uuid = @<e>-key-itemuuid.
			ENDIF.
		ENDLOOP.
	ENDMETHOD.

	METHOD if_abap_behavior_handler~delete.
		LOOP AT entities ASSIGNING FIELD-SYMBOL(<e>).
			IF <e>-entity = 'REQUEST'.
				DELETE FROM zreq_item WHERE client = @sy-mandt AND parent_uuid = @<e>-key-requestuuid.
				DELETE FROM zreq_hdr WHERE client = @sy-mandt AND req_uuid = @<e>-key-requestuuid.
			ELSEIF <e>-entity = 'ITEM'.
				DELETE FROM zreq_item WHERE client = @sy-mandt AND item_uuid = @<e>-key-itemuuid.
			ENDIF.
		ENDLOOP.
	ENDMETHOD.
ENDCLASS.