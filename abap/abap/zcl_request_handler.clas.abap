CLASS zcl_request_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_request_handler.

    " Константы для статусов
    CONSTANTS:
      BEGIN OF gc_status,
        draft     TYPE char20 VALUE 'DRAFT',
        submitted TYPE char20 VALUE 'SUBMITTED',
        approved  TYPE char20 VALUE 'APPROVED',
        rejected  TYPE char20 VALUE 'REJECTED',
        completed TYPE char20 VALUE 'COMPLETED',
      END OF gc_status.

    " Конструктор
    METHODS: constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    " Приватные методы
    METHODS:
      add_message
        IMPORTING
          iv_type    TYPE bapi_mtype
          iv_id      TYPE symsgid DEFAULT 'ZMM'
          iv_number  TYPE symsgno DEFAULT '001'
          iv_message TYPE bapi_msg
        CHANGING
          ct_messages TYPE zif_request_handler=>tt_message,

      lock_request
        IMPORTING
          iv_request_id TYPE sysuuid_x16
        RETURNING
          VALUE(rv_locked) TYPE abap_bool,

      unlock_request
        IMPORTING
          iv_request_id TYPE sysuuid_x16.

ENDCLASS.



CLASS zcl_request_handler IMPLEMENTATION.

  METHOD constructor.
    " Инициализация при необходимости
  ENDMETHOD.

  METHOD zif_request_handler~create_request.
    DATA: ls_req_hdr TYPE zreq_hdr,
          lt_req_item TYPE TABLE OF zreq_item,
          ls_req_item TYPE zreq_item.

    CLEAR: ev_request_id, et_messages.
    rv_success = abap_false.

    " Валидация данных
    IF zif_request_handler~validate_request( 
        is_request_header = is_request_header
        it_request_items = it_request_items ) = abap_false.
      RETURN.
    ENDIF.

    " Генерация ID запроса
    ev_request_id = cl_system_uuid=>create_uuid_x16_static( ).

    " Генерация номера запроса если не указан
    IF is_request_header-request_no IS INITIAL.
      ls_req_hdr-request_no = zif_request_handler~generate_request_number( ).
    ELSE.
      ls_req_hdr-request_no = is_request_header-request_no.
    ENDIF.

    " Заполнение заголовка
    ls_req_hdr-request_id = ev_request_id.
    ls_req_hdr-description = is_request_header-description.
    ls_req_hdr-department = is_request_header-department.
    ls_req_hdr-status = COND #( WHEN is_request_header-status IS INITIAL 
                                 THEN gc_status-draft 
                                 ELSE is_request_header-status ).
    ls_req_hdr-created_by = sy-uname.
    GET TIME STAMP FIELD ls_req_hdr-created_at.
    ls_req_hdr-changed_by = sy-uname.
    ls_req_hdr-changed_at = ls_req_hdr-created_at.

    " Сохранение заголовка
    INSERT zreq_hdr FROM ls_req_hdr.
    IF sy-subrc <> 0.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Ошибка при создании заголовка запроса'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Сохранение позиций
    LOOP AT it_request_items INTO DATA(ls_item).
      CLEAR ls_req_item.
      ls_req_item-request_id = ev_request_id.
      ls_req_item-item_id = cl_system_uuid=>create_uuid_x16_static( ).
      ls_req_item-position_no = ls_item-position_no.
      ls_req_item-material = ls_item-material.
      ls_req_item-description = ls_item-description.
      ls_req_item-quantity = ls_item-quantity.
      ls_req_item-unit = ls_item-unit.
      ls_req_item-price = ls_item-price.
      ls_req_item-currency = ls_item-currency.
      APPEND ls_req_item TO lt_req_item.
    ENDLOOP.

    IF lt_req_item IS NOT INITIAL.
      INSERT zreq_item FROM TABLE lt_req_item.
      IF sy-subrc <> 0.
        " Откат создания заголовка
        DELETE FROM zreq_hdr WHERE request_id = ev_request_id.
        add_message( EXPORTING iv_type = 'E'
                              iv_message = 'Ошибка при создании позиций запроса'
                    CHANGING ct_messages = et_messages ).
        RETURN.
      ENDIF.
    ENDIF.

    COMMIT WORK.
    
    add_message( EXPORTING iv_type = 'S'
                          iv_message = |Запрос { ls_req_hdr-request_no } успешно создан|
                CHANGING ct_messages = et_messages ).
    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_request_handler~read_request.
    DATA: ls_req_hdr TYPE zreq_hdr,
          lt_req_item TYPE TABLE OF zreq_item.

    CLEAR: es_request_full, et_messages.
    rv_success = abap_false.

    " Чтение заголовка
    SELECT SINGLE * FROM zreq_hdr INTO ls_req_hdr
      WHERE request_id = iv_request_id.
    
    IF sy-subrc <> 0.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос не найден'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Маппинг заголовка
    MOVE-CORRESPONDING ls_req_hdr TO es_request_full-header.

    " Чтение позиций
    SELECT * FROM zreq_item INTO TABLE lt_req_item
      WHERE request_id = iv_request_id
      ORDER BY position_no.

    " Маппинг позиций
    LOOP AT lt_req_item INTO DATA(ls_item).
      APPEND INITIAL LINE TO es_request_full-items ASSIGNING FIELD-SYMBOL(<ls_item>).
      MOVE-CORRESPONDING ls_item TO <ls_item>.
    ENDLOOP.

    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_request_handler~update_request.
    DATA: ls_req_hdr TYPE zreq_hdr.

    CLEAR: et_messages.
    rv_success = abap_false.

    " Блокировка запроса
    IF lock_request( is_request_header-request_id ) = abap_false.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос заблокирован другим пользователем'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Проверка существования запроса
    SELECT SINGLE * FROM zreq_hdr INTO ls_req_hdr
      WHERE request_id = is_request_header-request_id.
    
    IF sy-subrc <> 0.
      unlock_request( is_request_header-request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос не найден'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Валидация данных
    IF zif_request_handler~validate_request( 
        is_request_header = is_request_header
        it_request_items = it_request_items ) = abap_false.
      unlock_request( is_request_header-request_id ).
      RETURN.
    ENDIF.

    " Обновление заголовка
    ls_req_hdr-description = is_request_header-description.
    ls_req_hdr-department = is_request_header-department.
    ls_req_hdr-status = is_request_header-status.
    ls_req_hdr-changed_by = sy-uname.
    GET TIME STAMP FIELD ls_req_hdr-changed_at.

    UPDATE zreq_hdr FROM ls_req_hdr.
    IF sy-subrc <> 0.
      unlock_request( is_request_header-request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Ошибка при обновлении заголовка запроса'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Удаление старых позиций
    DELETE FROM zreq_item WHERE request_id = is_request_header-request_id.

    " Вставка новых позиций
    DATA: lt_req_item TYPE TABLE OF zreq_item,
          ls_req_item TYPE zreq_item.

    LOOP AT it_request_items INTO DATA(ls_item).
      CLEAR ls_req_item.
      ls_req_item-request_id = is_request_header-request_id.
      ls_req_item-item_id = COND #( WHEN ls_item-item_id IS INITIAL 
                                     THEN cl_system_uuid=>create_uuid_x16_static( )
                                     ELSE ls_item-item_id ).
      ls_req_item-position_no = ls_item-position_no.
      ls_req_item-material = ls_item-material.
      ls_req_item-description = ls_item-description.
      ls_req_item-quantity = ls_item-quantity.
      ls_req_item-unit = ls_item-unit.
      ls_req_item-price = ls_item-price.
      ls_req_item-currency = ls_item-currency.
      APPEND ls_req_item TO lt_req_item.
    ENDLOOP.

    IF lt_req_item IS NOT INITIAL.
      INSERT zreq_item FROM TABLE lt_req_item.
      IF sy-subrc <> 0.
        unlock_request( is_request_header-request_id ).
        add_message( EXPORTING iv_type = 'E'
                              iv_message = 'Ошибка при обновлении позиций запроса'
                    CHANGING ct_messages = et_messages ).
        RETURN.
      ENDIF.
    ENDIF.

    COMMIT WORK.
    unlock_request( is_request_header-request_id ).
    
    add_message( EXPORTING iv_type = 'S'
                          iv_message = |Запрос { ls_req_hdr-request_no } успешно обновлен|
                CHANGING ct_messages = et_messages ).
    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_request_handler~delete_request.
    CLEAR: et_messages.
    rv_success = abap_false.

    " Блокировка запроса
    IF lock_request( iv_request_id ) = abap_false.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос заблокирован другим пользователем'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Проверка существования запроса
    SELECT SINGLE request_no FROM zreq_hdr INTO @DATA(lv_request_no)
      WHERE request_id = @iv_request_id.
    
    IF sy-subrc <> 0.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос не найден'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Удаление позиций
    DELETE FROM zreq_item WHERE request_id = iv_request_id.

    " Удаление заголовка
    DELETE FROM zreq_hdr WHERE request_id = iv_request_id.
    IF sy-subrc <> 0.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Ошибка при удалении запроса'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    COMMIT WORK.
    unlock_request( iv_request_id ).
    
    add_message( EXPORTING iv_type = 'S'
                          iv_message = |Запрос { lv_request_no } успешно удален|
                CHANGING ct_messages = et_messages ).
    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_request_handler~get_request_list.
    DATA: lt_req_hdr TYPE TABLE OF zreq_hdr.

    CLEAR: et_requests, et_messages.
    rv_success = abap_false.

    " Построение динамического WHERE условия
    DATA(lv_where) = |status <> ''|.
    
    IF iv_department IS NOT INITIAL.
      lv_where = |{ lv_where } AND department = '{ iv_department }'|.
    ENDIF.
    
    IF iv_status IS NOT INITIAL.
      lv_where = |{ lv_where } AND status = '{ iv_status }'|.
    ENDIF.
    
    IF iv_created_by IS NOT INITIAL.
      lv_where = |{ lv_where } AND created_by = '{ iv_created_by }'|.
    ENDIF.

    " Выборка данных
    SELECT * FROM zreq_hdr INTO TABLE lt_req_hdr
      WHERE (lv_where)
      ORDER BY created_at DESCENDING.

    " Маппинг данных
    LOOP AT lt_req_hdr INTO DATA(ls_hdr).
      APPEND INITIAL LINE TO et_requests ASSIGNING FIELD-SYMBOL(<ls_request>).
      MOVE-CORRESPONDING ls_hdr TO <ls_request>.
    ENDLOOP.

    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_request_handler~approve_request.
    DATA: ls_req_hdr TYPE zreq_hdr.

    CLEAR: et_messages.
    rv_success = abap_false.

    " Блокировка запроса
    IF lock_request( iv_request_id ) = abap_false.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос заблокирован другим пользователем'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Чтение запроса
    SELECT SINGLE * FROM zreq_hdr INTO ls_req_hdr
      WHERE request_id = iv_request_id.
    
    IF sy-subrc <> 0.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос не найден'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Проверка статуса
    IF ls_req_hdr-status <> gc_status-submitted.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос должен быть в статусе "Подан" для утверждения'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Обновление статуса
    ls_req_hdr-status = gc_status-approved.
    ls_req_hdr-changed_by = iv_approver.
    GET TIME STAMP FIELD ls_req_hdr-changed_at.

    UPDATE zreq_hdr FROM ls_req_hdr.
    IF sy-subrc <> 0.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Ошибка при утверждении запроса'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    COMMIT WORK.
    unlock_request( iv_request_id ).
    
    add_message( EXPORTING iv_type = 'S'
                          iv_message = |Запрос { ls_req_hdr-request_no } успешно утвержден|
                CHANGING ct_messages = et_messages ).
    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_request_handler~reject_request.
    DATA: ls_req_hdr TYPE zreq_hdr.

    CLEAR: et_messages.
    rv_success = abap_false.

    " Блокировка запроса
    IF lock_request( iv_request_id ) = abap_false.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос заблокирован другим пользователем'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Чтение запроса
    SELECT SINGLE * FROM zreq_hdr INTO ls_req_hdr
      WHERE request_id = iv_request_id.
    
    IF sy-subrc <> 0.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос не найден'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Проверка статуса
    IF ls_req_hdr-status <> gc_status-submitted.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос должен быть в статусе "Подан" для отклонения'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    " Обновление статуса
    ls_req_hdr-status = gc_status-rejected.
    ls_req_hdr-changed_by = sy-uname.
    GET TIME STAMP FIELD ls_req_hdr-changed_at.

    UPDATE zreq_hdr FROM ls_req_hdr.
    IF sy-subrc <> 0.
      unlock_request( iv_request_id ).
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Ошибка при отклонении запроса'
                  CHANGING ct_messages = et_messages ).
      RETURN.
    ENDIF.

    COMMIT WORK.
    unlock_request( iv_request_id ).
    
    add_message( EXPORTING iv_type = 'S'
                          iv_message = |Запрос { ls_req_hdr-request_no } отклонен. Причина: { iv_reason }|
                CHANGING ct_messages = et_messages ).
    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_request_handler~generate_request_number.
    DATA: lv_number TYPE numc10,
          lv_year   TYPE char4.

    " Получение текущего года
    lv_year = sy-datum(4).

    " Получение последнего номера запроса за текущий год
    SELECT MAX( request_no ) FROM zreq_hdr INTO @DATA(lv_last_no)
      WHERE request_no LIKE @( |{ lv_year }%| ).

    IF lv_last_no IS INITIAL.
      lv_number = 1.
    ELSE.
      lv_number = lv_last_no+4 + 1.
    ENDIF.

    " Формирование номера: YYYYNNNNNN
    rv_request_no = |{ lv_year }{ lv_number ALPHA = IN WIDTH = 6 }|.

  ENDMETHOD.

  METHOD zif_request_handler~validate_request.
    CLEAR: et_messages.
    rv_valid = abap_true.

    " Проверка обязательных полей заголовка
    IF is_request_header-description IS INITIAL.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Описание запроса обязательно для заполнения'
                  CHANGING ct_messages = et_messages ).
      rv_valid = abap_false.
    ENDIF.

    IF is_request_header-department IS INITIAL.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Подразделение обязательно для заполнения'
                  CHANGING ct_messages = et_messages ).
      rv_valid = abap_false.
    ENDIF.

    " Проверка позиций
    IF it_request_items IS INITIAL.
      add_message( EXPORTING iv_type = 'E'
                            iv_message = 'Запрос должен содержать хотя бы одну позицию'
                  CHANGING ct_messages = et_messages ).
      rv_valid = abap_false.
    ELSE.
      " Проверка каждой позиции
      LOOP AT it_request_items INTO DATA(ls_item).
        IF ls_item-description IS INITIAL AND ls_item-material IS INITIAL.
          add_message( EXPORTING iv_type = 'E'
                                iv_message = |Позиция { ls_item-position_no }: необходимо указать материал или описание|
                      CHANGING ct_messages = et_messages ).
          rv_valid = abap_false.
        ENDIF.

        IF ls_item-quantity <= 0.
          add_message( EXPORTING iv_type = 'E'
                                iv_message = |Позиция { ls_item-position_no }: количество должно быть больше нуля|
                      CHANGING ct_messages = et_messages ).
          rv_valid = abap_false.
        ENDIF.

        IF ls_item-unit IS INITIAL.
          add_message( EXPORTING iv_type = 'E'
                                iv_message = |Позиция { ls_item-position_no }: единица измерения обязательна|
                      CHANGING ct_messages = et_messages ).
          rv_valid = abap_false.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD add_message.
    DATA: ls_message TYPE zif_request_handler=>ty_message.

    ls_message-type = iv_type.
    ls_message-id = iv_id.
    ls_message-number = iv_number.
    ls_message-message = iv_message.
    APPEND ls_message TO ct_messages.

  ENDMETHOD.

  METHOD lock_request.
    " Здесь должна быть логика блокировки через ENQUEUE
    " Для примера возвращаем успех
    rv_locked = abap_true.
  ENDMETHOD.

  METHOD unlock_request.
    " Здесь должна быть логика разблокировки через DEQUEUE
  ENDMETHOD.

ENDCLASS.