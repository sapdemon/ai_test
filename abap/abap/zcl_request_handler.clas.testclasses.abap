*"* use this source file for your ABAP unit test classes
CLASS ltcl_request_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_request_handler.  " Class Under Test

    METHODS:
      setup,
      teardown,
      test_create_request FOR TESTING,
      test_read_request FOR TESTING,
      test_update_request FOR TESTING,
      test_delete_request FOR TESTING,
      test_approve_request FOR TESTING,
      test_reject_request FOR TESTING,
      test_generate_request_no FOR TESTING,
      test_validate_request FOR TESTING,
      test_get_request_list FOR TESTING.

    METHODS:
      create_test_header
        RETURNING VALUE(rs_header) TYPE zif_request_handler=>ty_request_header,
      create_test_items
        RETURNING VALUE(rt_items) TYPE zif_request_handler=>tt_request_item.

ENDCLASS.

CLASS ltcl_request_handler IMPLEMENTATION.

  METHOD setup.
    " Создание экземпляра класса для тестирования
    mo_cut = NEW #( ).
    
    " Очистка тестовых данных
    DELETE FROM zreq_hdr WHERE created_by = 'UNITTEST'.
    DELETE FROM zreq_item WHERE request_id IN 
      ( SELECT request_id FROM zreq_hdr WHERE created_by = 'UNITTEST' ).
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD teardown.
    " Очистка тестовых данных после теста
    DELETE FROM zreq_hdr WHERE created_by = 'UNITTEST'.
    DELETE FROM zreq_item WHERE request_id IN 
      ( SELECT request_id FROM zreq_hdr WHERE created_by = 'UNITTEST' ).
    COMMIT WORK AND WAIT.
    
    CLEAR mo_cut.
  ENDMETHOD.

  METHOD test_create_request.
    DATA: lv_request_id TYPE sysuuid_x16,
          lt_messages   TYPE zif_request_handler=>tt_message,
          ls_header     TYPE zif_request_handler=>ty_request_header,
          lt_items      TYPE zif_request_handler=>tt_request_item,
          lv_success    TYPE abap_bool.

    " Подготовка тестовых данных
    ls_header = create_test_header( ).
    lt_items = create_test_items( ).

    " Выполнение тестируемого метода
    lv_success = mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = lt_items
      IMPORTING
        ev_request_id     = lv_request_id
        et_messages       = lt_messages
    ).

    " Проверка результатов
    cl_abap_unit_assert=>assert_equals( 
      act = lv_success 
      exp = abap_true 
      msg = 'Создание запроса должно быть успешным' ).
      
    cl_abap_unit_assert=>assert_not_initial( 
      act = lv_request_id 
      msg = 'ID запроса должен быть заполнен' ).

    " Проверка, что запрос действительно создан в БД
    SELECT SINGLE request_id FROM zreq_hdr INTO @DATA(lv_check_id)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_subrc( 
      exp = 0 
      msg = 'Запрос должен быть найден в базе данных' ).

    " Проверка создания позиций
    SELECT COUNT(*) FROM zreq_item INTO @DATA(lv_item_count)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_equals( 
      act = lv_item_count 
      exp = lines( lt_items ) 
      msg = 'Количество созданных позиций должно соответствовать' ).

  ENDMETHOD.

  METHOD test_read_request.
    DATA: lv_request_id   TYPE sysuuid_x16,
          lt_messages     TYPE zif_request_handler=>tt_message,
          ls_request_full TYPE zif_request_handler=>ty_request_full,
          lv_success      TYPE abap_bool.

    " Создание тестового запроса
    mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = create_test_header( )
        it_request_items  = create_test_items( )
      IMPORTING
        ev_request_id     = lv_request_id
    ).

    " Чтение созданного запроса
    lv_success = mo_cut->zif_request_handler~read_request(
      EXPORTING
        iv_request_id   = lv_request_id
      IMPORTING
        es_request_full = ls_request_full
        et_messages     = lt_messages
    ).

    " Проверка результатов
    cl_abap_unit_assert=>assert_equals( 
      act = lv_success 
      exp = abap_true 
      msg = 'Чтение запроса должно быть успешным' ).
      
    cl_abap_unit_assert=>assert_not_initial( 
      act = ls_request_full-header 
      msg = 'Заголовок запроса должен быть заполнен' ).
      
    cl_abap_unit_assert=>assert_equals( 
      act = lines( ls_request_full-items ) 
      exp = 2 
      msg = 'Должно быть прочитано 2 позиции' ).

  ENDMETHOD.

  METHOD test_update_request.
    DATA: lv_request_id TYPE sysuuid_x16,
          lt_messages   TYPE zif_request_handler=>tt_message,
          ls_header     TYPE zif_request_handler=>ty_request_header,
          lt_items      TYPE zif_request_handler=>tt_request_item,
          lv_success    TYPE abap_bool.

    " Создание тестового запроса
    mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = create_test_header( )
        it_request_items  = create_test_items( )
      IMPORTING
        ev_request_id     = lv_request_id
    ).

    " Подготовка данных для обновления
    ls_header = create_test_header( ).
    ls_header-request_id = lv_request_id.
    ls_header-description = 'Обновленное описание'.
    ls_header-status = zcl_request_handler=>gc_status-submitted.

    lt_items = create_test_items( ).
    " Добавим третью позицию
    APPEND INITIAL LINE TO lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    <ls_item>-position_no = '003'.
    <ls_item>-description = 'Новая позиция'.
    <ls_item>-quantity = 5.
    <ls_item>-unit = 'ST'.

    " Выполнение обновления
    lv_success = mo_cut->zif_request_handler~update_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = lt_items
      IMPORTING
        et_messages       = lt_messages
    ).

    " Проверка результатов
    cl_abap_unit_assert=>assert_equals( 
      act = lv_success 
      exp = abap_true 
      msg = 'Обновление запроса должно быть успешным' ).

    " Проверка обновленных данных
    SELECT SINGLE description, status FROM zreq_hdr 
      INTO @DATA(ls_check)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_equals( 
      act = ls_check-description 
      exp = 'Обновленное описание' 
      msg = 'Описание должно быть обновлено' ).
      
    cl_abap_unit_assert=>assert_equals( 
      act = ls_check-status 
      exp = zcl_request_handler=>gc_status-submitted 
      msg = 'Статус должен быть обновлен' ).

    " Проверка количества позиций
    SELECT COUNT(*) FROM zreq_item INTO @DATA(lv_item_count)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_equals( 
      act = lv_item_count 
      exp = 3 
      msg = 'Должно быть 3 позиции после обновления' ).

  ENDMETHOD.

  METHOD test_delete_request.
    DATA: lv_request_id TYPE sysuuid_x16,
          lt_messages   TYPE zif_request_handler=>tt_message,
          lv_success    TYPE abap_bool.

    " Создание тестового запроса
    mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = create_test_header( )
        it_request_items  = create_test_items( )
      IMPORTING
        ev_request_id     = lv_request_id
    ).

    " Удаление запроса
    lv_success = mo_cut->zif_request_handler~delete_request(
      EXPORTING
        iv_request_id = lv_request_id
      IMPORTING
        et_messages   = lt_messages
    ).

    " Проверка результатов
    cl_abap_unit_assert=>assert_equals( 
      act = lv_success 
      exp = abap_true 
      msg = 'Удаление запроса должно быть успешным' ).

    " Проверка, что запрос удален из БД
    SELECT SINGLE request_id FROM zreq_hdr INTO @DATA(lv_check_id)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_subrc( 
      exp = 4 
      msg = 'Запрос не должен быть найден в базе данных' ).

    " Проверка удаления позиций
    SELECT COUNT(*) FROM zreq_item INTO @DATA(lv_item_count)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_equals( 
      act = lv_item_count 
      exp = 0 
      msg = 'Все позиции должны быть удалены' ).

  ENDMETHOD.

  METHOD test_approve_request.
    DATA: lv_request_id TYPE sysuuid_x16,
          lt_messages   TYPE zif_request_handler=>tt_message,
          ls_header     TYPE zif_request_handler=>ty_request_header,
          lv_success    TYPE abap_bool.

    " Создание тестового запроса в статусе SUBMITTED
    ls_header = create_test_header( ).
    ls_header-status = zcl_request_handler=>gc_status-submitted.
    
    mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = create_test_items( )
      IMPORTING
        ev_request_id     = lv_request_id
    ).

    " Утверждение запроса
    lv_success = mo_cut->zif_request_handler~approve_request(
      EXPORTING
        iv_request_id = lv_request_id
        iv_approver   = 'APPROVER'
      IMPORTING
        et_messages   = lt_messages
    ).

    " Проверка результатов
    cl_abap_unit_assert=>assert_equals( 
      act = lv_success 
      exp = abap_true 
      msg = 'Утверждение запроса должно быть успешным' ).

    " Проверка статуса
    SELECT SINGLE status FROM zreq_hdr INTO @DATA(lv_status)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_equals( 
      act = lv_status 
      exp = zcl_request_handler=>gc_status-approved 
      msg = 'Статус должен быть APPROVED' ).

  ENDMETHOD.

  METHOD test_reject_request.
    DATA: lv_request_id TYPE sysuuid_x16,
          lt_messages   TYPE zif_request_handler=>tt_message,
          ls_header     TYPE zif_request_handler=>ty_request_header,
          lv_success    TYPE abap_bool.

    " Создание тестового запроса в статусе SUBMITTED
    ls_header = create_test_header( ).
    ls_header-status = zcl_request_handler=>gc_status-submitted.
    
    mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = create_test_items( )
      IMPORTING
        ev_request_id     = lv_request_id
    ).

    " Отклонение запроса
    lv_success = mo_cut->zif_request_handler~reject_request(
      EXPORTING
        iv_request_id = lv_request_id
        iv_reason     = 'Недостаточное обоснование'
      IMPORTING
        et_messages   = lt_messages
    ).

    " Проверка результатов
    cl_abap_unit_assert=>assert_equals( 
      act = lv_success 
      exp = abap_true 
      msg = 'Отклонение запроса должно быть успешным' ).

    " Проверка статуса
    SELECT SINGLE status FROM zreq_hdr INTO @DATA(lv_status)
      WHERE request_id = @lv_request_id.
      
    cl_abap_unit_assert=>assert_equals( 
      act = lv_status 
      exp = zcl_request_handler=>gc_status-rejected 
      msg = 'Статус должен быть REJECTED' ).

  ENDMETHOD.

  METHOD test_generate_request_no.
    DATA: lv_request_no TYPE char10,
          lv_year       TYPE char4.

    " Получение номера запроса
    lv_request_no = mo_cut->zif_request_handler~generate_request_number( ).

    " Проверка формата
    cl_abap_unit_assert=>assert_not_initial( 
      act = lv_request_no 
      msg = 'Номер запроса должен быть сгенерирован' ).

    lv_year = sy-datum(4).
    cl_abap_unit_assert=>assert_equals( 
      act = lv_request_no(4) 
      exp = lv_year 
      msg = 'Номер должен начинаться с текущего года' ).

    cl_abap_unit_assert=>assert_equals( 
      act = strlen( lv_request_no ) 
      exp = 10 
      msg = 'Длина номера должна быть 10 символов' ).

  ENDMETHOD.

  METHOD test_validate_request.
    DATA: ls_header   TYPE zif_request_handler=>ty_request_header,
          lt_items    TYPE zif_request_handler=>tt_request_item,
          lt_messages TYPE zif_request_handler=>tt_message,
          lv_valid    TYPE abap_bool.

    " Тест 1: Валидный запрос
    ls_header = create_test_header( ).
    lt_items = create_test_items( ).
    
    lv_valid = mo_cut->zif_request_handler~validate_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = lt_items
      IMPORTING
        et_messages       = lt_messages
    ).
    
    cl_abap_unit_assert=>assert_equals( 
      act = lv_valid 
      exp = abap_true 
      msg = 'Валидный запрос должен пройти проверку' ).

    " Тест 2: Запрос без описания
    CLEAR: ls_header-description, lt_messages.
    
    lv_valid = mo_cut->zif_request_handler~validate_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = lt_items
      IMPORTING
        et_messages       = lt_messages
    ).
    
    cl_abap_unit_assert=>assert_equals( 
      act = lv_valid 
      exp = abap_false 
      msg = 'Запрос без описания не должен пройти проверку' ).

    " Тест 3: Запрос без позиций
    ls_header = create_test_header( ).
    CLEAR: lt_items, lt_messages.
    
    lv_valid = mo_cut->zif_request_handler~validate_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = lt_items
      IMPORTING
        et_messages       = lt_messages
    ).
    
    cl_abap_unit_assert=>assert_equals( 
      act = lv_valid 
      exp = abap_false 
      msg = 'Запрос без позиций не должен пройти проверку' ).

  ENDMETHOD.

  METHOD test_get_request_list.
    DATA: lt_requests TYPE zif_request_handler=>tt_request_header,
          lt_messages TYPE zif_request_handler=>tt_message,
          ls_header   TYPE zif_request_handler=>ty_request_header,
          lv_success  TYPE abap_bool.

    " Создание нескольких тестовых запросов
    ls_header = create_test_header( ).
    ls_header-department = 'IT'.
    ls_header-status = zcl_request_handler=>gc_status-draft.
    mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = create_test_items( )
    ).

    ls_header-department = 'HR'.
    ls_header-status = zcl_request_handler=>gc_status-submitted.
    mo_cut->zif_request_handler~create_request(
      EXPORTING
        is_request_header = ls_header
        it_request_items  = create_test_items( )
    ).

    " Тест 1: Получение всех запросов
    lv_success = mo_cut->zif_request_handler~get_request_list(
      IMPORTING
        et_requests = lt_requests
        et_messages = lt_messages
    ).
    
    cl_abap_unit_assert=>assert_equals( 
      act = lv_success 
      exp = abap_true 
      msg = 'Получение списка должно быть успешным' ).
    
    cl_abap_unit_assert=>assert_true( 
      act = lines( lt_requests ) >= 2 
      msg = 'Должно быть минимум 2 запроса' ).

    " Тест 2: Фильтр по подразделению
    CLEAR lt_requests.
    lv_success = mo_cut->zif_request_handler~get_request_list(
      EXPORTING
        iv_department = 'IT'
      IMPORTING
        et_requests = lt_requests
        et_messages = lt_messages
    ).
    
    LOOP AT lt_requests INTO DATA(ls_req).
      cl_abap_unit_assert=>assert_equals( 
        act = ls_req-department 
        exp = 'IT' 
        msg = 'Все запросы должны быть из подразделения IT' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD create_test_header.
    rs_header-description = 'Тестовый запрос'.
    rs_header-department = 'IT'.
    rs_header-status = zcl_request_handler=>gc_status-draft.
    rs_header-created_by = 'UNITTEST'.
  ENDMETHOD.

  METHOD create_test_items.
    APPEND INITIAL LINE TO rt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    <ls_item>-position_no = '001'.
    <ls_item>-material = '100-100'.
    <ls_item>-description = 'Компьютер'.
    <ls_item>-quantity = 2.
    <ls_item>-unit = 'ST'.
    <ls_item>-price = '50000.00'.
    <ls_item>-currency = 'RUB'.

    APPEND INITIAL LINE TO rt_items ASSIGNING <ls_item>.
    <ls_item>-position_no = '002'.
    <ls_item>-description = 'Монитор'.
    <ls_item>-quantity = 2.
    <ls_item>-unit = 'ST'.
    <ls_item>-price = '15000.00'.
    <ls_item>-currency = 'RUB'.
  ENDMETHOD.

ENDCLASS.