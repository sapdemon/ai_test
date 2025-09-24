*&---------------------------------------------------------------------*
*& Report Z_REQUEST_EXAMPLE
*&---------------------------------------------------------------------*
*& Пример использования класса ZCL_REQUEST_HANDLER
*&---------------------------------------------------------------------*
REPORT z_request_example.

" Объявление переменных
DATA: go_request_handler TYPE REF TO zcl_request_handler,
      gs_request_header  TYPE zif_request_handler=>ty_request_header,
      gt_request_items   TYPE zif_request_handler=>tt_request_item,
      gs_request_full    TYPE zif_request_handler=>ty_request_full,
      gt_requests        TYPE zif_request_handler=>tt_request_header,
      gt_messages        TYPE zif_request_handler=>tt_message,
      gv_request_id      TYPE sysuuid_x16,
      gv_success         TYPE abap_bool.

" Параметры выбора
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_action TYPE i DEFAULT 1.
SELECTION-SCREEN COMMENT /1(60) TEXT-002.
SELECTION-SCREEN COMMENT /1(60) TEXT-003.
SELECTION-SCREEN COMMENT /1(60) TEXT-004.
SELECTION-SCREEN COMMENT /1(60) TEXT-005.
SELECTION-SCREEN COMMENT /1(60) TEXT-006.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  " Создание экземпляра обработчика
  CREATE OBJECT go_request_handler.

  CASE p_action.
    WHEN 1.
      PERFORM create_request.
    WHEN 2.
      PERFORM read_request.
    WHEN 3.
      PERFORM update_request.
    WHEN 4.
      PERFORM delete_request.
    WHEN 5.
      PERFORM list_requests.
    WHEN OTHERS.
      MESSAGE 'Неверное действие' TYPE 'E'.
  ENDCASE.

  " Вывод сообщений
  PERFORM display_messages.

*&---------------------------------------------------------------------*
*& Form CREATE_REQUEST
*&---------------------------------------------------------------------*
FORM create_request.
  " Подготовка данных заголовка
  gs_request_header-description = 'Запрос на закупку офисного оборудования'.
  gs_request_header-department = 'IT'.

  " Подготовка позиций
  APPEND INITIAL LINE TO gt_request_items ASSIGNING FIELD-SYMBOL(<ls_item>).
  <ls_item>-position_no = '001'.
  <ls_item>-material = '100-200'.
  <ls_item>-description = 'Ноутбук Dell Latitude'.
  <ls_item>-quantity = 5.
  <ls_item>-unit = 'ST'.
  <ls_item>-price = '75000.00'.
  <ls_item>-currency = 'RUB'.

  APPEND INITIAL LINE TO gt_request_items ASSIGNING <ls_item>.
  <ls_item>-position_no = '002'.
  <ls_item>-description = 'Мышь беспроводная'.
  <ls_item>-quantity = 5.
  <ls_item>-unit = 'ST'.
  <ls_item>-price = '1500.00'.
  <ls_item>-currency = 'RUB'.

  APPEND INITIAL LINE TO gt_request_items ASSIGNING <ls_item>.
  <ls_item>-position_no = '003'.
  <ls_item>-material = '200-300'.
  <ls_item>-description = 'Клавиатура механическая'.
  <ls_item>-quantity = 5.
  <ls_item>-unit = 'ST'.
  <ls_item>-price = '5000.00'.
  <ls_item>-currency = 'RUB'.

  " Создание запроса
  gv_success = go_request_handler->zif_request_handler~create_request(
    EXPORTING
      is_request_header = gs_request_header
      it_request_items  = gt_request_items
    IMPORTING
      ev_request_id     = gv_request_id
      et_messages       = gt_messages
  ).

  IF gv_success = abap_true.
    WRITE: / 'Запрос успешно создан!'.
    WRITE: / 'ID запроса:', gv_request_id.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_REQUEST
*&---------------------------------------------------------------------*
FORM read_request.
  " Для примера читаем последний созданный запрос
  SELECT request_id FROM zreq_hdr
    INTO @gv_request_id
    UP TO 1 ROWS
    ORDER BY created_at DESCENDING.
  ENDSELECT.

  IF sy-subrc = 0.
    gv_success = go_request_handler->zif_request_handler~read_request(
      EXPORTING
        iv_request_id   = gv_request_id
      IMPORTING
        es_request_full = gs_request_full
        et_messages     = gt_messages
    ).

    IF gv_success = abap_true.
      WRITE: / 'Информация о запросе:'.
      WRITE: / 'Номер:', gs_request_full-header-request_no.
      WRITE: / 'Описание:', gs_request_full-header-description.
      WRITE: / 'Подразделение:', gs_request_full-header-department.
      WRITE: / 'Статус:', gs_request_full-header-status.
      WRITE: / 'Создан:', gs_request_full-header-created_by, 
               gs_request_full-header-created_at.
      
      SKIP.
      WRITE: / 'Позиции:'.
      LOOP AT gs_request_full-items INTO DATA(ls_item).
        WRITE: / ls_item-position_no,
                 ls_item-description,
                 ls_item-quantity,
                 ls_item-unit,
                 ls_item-price,
                 ls_item-currency.
      ENDLOOP.
    ENDIF.
  ELSE.
    WRITE: / 'Нет запросов для чтения'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_REQUEST
*&---------------------------------------------------------------------*
FORM update_request.
  " Для примера обновляем последний созданный запрос
  SELECT request_id FROM zreq_hdr
    INTO @gv_request_id
    UP TO 1 ROWS
    ORDER BY created_at DESCENDING.
  ENDSELECT.

  IF sy-subrc = 0.
    " Сначала читаем текущие данные
    gv_success = go_request_handler->zif_request_handler~read_request(
      EXPORTING
        iv_request_id   = gv_request_id
      IMPORTING
        es_request_full = gs_request_full
        et_messages     = gt_messages
    ).

    IF gv_success = abap_true.
      " Изменяем данные
      gs_request_full-header-description = 'ОБНОВЛЕНО: ' && gs_request_full-header-description.
      gs_request_full-header-status = zcl_request_handler=>gc_status-submitted.

      " Добавляем новую позицию
      APPEND INITIAL LINE TO gs_request_full-items ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-position_no = '004'.
      <ls_item>-description = 'Дополнительная позиция'.
      <ls_item>-quantity = 1.
      <ls_item>-unit = 'ST'.
      <ls_item>-price = '10000.00'.
      <ls_item>-currency = 'RUB'.

      " Обновляем запрос
      gv_success = go_request_handler->zif_request_handler~update_request(
        EXPORTING
          is_request_header = gs_request_full-header
          it_request_items  = gs_request_full-items
        IMPORTING
          et_messages       = gt_messages
      ).

      IF gv_success = abap_true.
        WRITE: / 'Запрос успешно обновлен!'.
      ENDIF.
    ENDIF.
  ELSE.
    WRITE: / 'Нет запросов для обновления'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_REQUEST
*&---------------------------------------------------------------------*
FORM delete_request.
  " Для примера удаляем последний созданный запрос
  SELECT request_id FROM zreq_hdr
    INTO @gv_request_id
    UP TO 1 ROWS
    WHERE status = @zcl_request_handler=>gc_status-draft
    ORDER BY created_at DESCENDING.
  ENDSELECT.

  IF sy-subrc = 0.
    gv_success = go_request_handler->zif_request_handler~delete_request(
      EXPORTING
        iv_request_id = gv_request_id
      IMPORTING
        et_messages   = gt_messages
    ).

    IF gv_success = abap_true.
      WRITE: / 'Запрос успешно удален!'.
    ENDIF.
  ELSE.
    WRITE: / 'Нет запросов для удаления (только черновики можно удалять)'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LIST_REQUESTS
*&---------------------------------------------------------------------*
FORM list_requests.
  " Получение списка всех запросов
  gv_success = go_request_handler->zif_request_handler~get_request_list(
    IMPORTING
      et_requests = gt_requests
      et_messages = gt_messages
  ).

  IF gv_success = abap_true.
    WRITE: / 'Список запросов:'.
    WRITE: / '№', 'Номер запроса', 'Описание', 'Подразделение', 'Статус', 'Создан'.
    ULINE.
    
    LOOP AT gt_requests INTO DATA(ls_request).
      WRITE: / sy-tabix,
               ls_request-request_no,
               ls_request-description(30),
               ls_request-department,
               ls_request-status,
               ls_request-created_by.
    ENDLOOP.
    
    SKIP.
    WRITE: / 'Всего запросов:', lines( gt_requests ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
FORM display_messages.
  IF gt_messages IS NOT INITIAL.
    SKIP.
    WRITE: / 'Сообщения:'.
    LOOP AT gt_messages INTO DATA(ls_message).
      CASE ls_message-type.
        WHEN 'E'.
          WRITE: / icon_led_red AS ICON, ls_message-message.
        WHEN 'W'.
          WRITE: / icon_led_yellow AS ICON, ls_message-message.
        WHEN 'S'.
          WRITE: / icon_led_green AS ICON, ls_message-message.
        WHEN OTHERS.
          WRITE: / ls_message-message.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.