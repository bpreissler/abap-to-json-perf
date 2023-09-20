CLASS z_json_perf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS start_measurement RETURNING VALUE(runtime) TYPE i.
    METHODS end_measurement RETURNING VALUE(runtime) TYPE i.
    METHODS store_runtime IMPORTING old TYPE i new TYPE i name TYPE string.
    TYPES: BEGIN OF runtime,
             name       TYPE string,
             old        TYPE i,
             new        TYPE i,
             diff       TYPE i,
             percent(5) TYPE p DECIMALS 2,
           END OF runtime.
    DATA runtimes TYPE HASHED TABLE OF runtime WITH UNIQUE KEY name.

    METHODS perf_timestamp.
    METHODS perf_sbook.
    METHODS perf_all_types.

ENDCLASS.



CLASS z_json_perf IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    perf_timestamp( ).
    perf_sbook( ).
    perf_all_types( ).
    out->write( data = runtimes name = `Runtimes` ).
  ENDMETHOD.

  METHOD end_measurement.
    GET RUN TIME FIELD runtime.
  ENDMETHOD.

  METHOD start_measurement.
    GET RUN TIME FIELD runtime.
  ENDMETHOD.

  METHOD store_runtime.
    INSERT VALUE #( name = name old = old new = new diff = old - new percent = ( old - new ) / old * 100 ) INTO TABLE runtimes.
  ENDMETHOD.

  METHOD perf_all_types.
    DATA: BEGIN OF test,
            id         TYPE i,
            utclong    TYPE utclong,
            timestamp  TYPE timestamp,
            timestampl TYPE timestampl,
            int1       TYPE int1,
            int2       TYPE int2,
            int4       TYPE int4,
            int8       TYPE int8,
            packed(8)  TYPE p DECIMALS 2,
            decfloat16 TYPE decfloat16,
            decfloat34 TYPE decfloat34,
            fp         TYPE f,
            char(10)   TYPE c,
            n(8)       TYPE n,
            string     TYPE string,
            x(12)      TYPE x,
            xstring    TYPE xstring,
            date       TYPE d,
            time       TYPE t,
          END OF test,
          tests LIKE STANDARD TABLE OF test.

    DO 10000 TIMES.
      "Every 20th row is empty
      IF (  sy-index MOD 20  = 0 ).
        INSERT VALUE #( id = sy-index ) INTO TABLE tests.
      ELSE.
        test-id = sy-index.
        test-utclong = utclong_current( ).
        GET TIME STAMP FIELD test-timestamp.
        GET TIME STAMP FIELD test-timestampl.
        test-int1 = 1.
        test-int2 = 2.
        test-int4 = 4.
        test-int8 = 8.
        test-packed = '123.45'.
        test-decfloat16 = 12345.
        test-decfloat34 = 123412342134.
        test-fp = 234.
        test-char = 'TestiTest'.
        test-n = '00000123'.
        test-string = `Testi Test`.
        test-x = z_ui2_json=>string_to_raw( iv_string = `Test` ).
        test-xstring =  z_ui2_json=>string_to_raw( iv_string = `Testi Test` ).
        test-date = sy-datum.
        test-time = sy-timlo.
        INSERT test INTO TABLE tests.
      ENDIF.
    ENDDO.

    DATA(start1) = start_measurement( ).
    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data          = tests
        ts_as_iso8601 = abap_true
      RECEIVING
        r_json        = DATA(json1).
    DATA(end1) = end_measurement( ).

    DATA(start2) = start_measurement( ).
    CALL METHOD z_ui2_json=>serialize
      EXPORTING
        data          = tests
        ts_as_iso8601 = abap_true
      RECEIVING
        r_json        = DATA(json2).
    DATA(end2) = end_measurement( ).

    store_runtime( name = `All Types` old = end1 - start1 new = end2 - start2 ).

    IF ( json1 NE json2 ).
      RAISE EXCEPTION TYPE z_ex_json_perf.
    ENDIF.
  ENDMETHOD.

  METHOD perf_sbook.
    SELECT * FROM sbook  UP TO 20000 ROWS INTO TABLE @DATA(sbook).
    DATA(start1) = start_measurement( ).
    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = sbook
      RECEIVING
        r_json = DATA(json1).
    DATA(end1) = end_measurement( ).

    DATA(start2) = start_measurement( ).
    CALL METHOD z_ui2_json=>serialize
      EXPORTING
        data   = sbook
      RECEIVING
        r_json = DATA(json2).
    DATA(end2) = end_measurement( ).

    store_runtime( name = |SBOOK lines { lines( sbook ) }|  old = end1 - start1 new = end2 - start2 ).

    IF ( json1 NE json2 ).
      RAISE EXCEPTION TYPE z_ex_json_perf.
    ENDIF.

  ENDMETHOD.

  METHOD perf_timestamp.
    DATA: BEGIN OF timestamp_line,
            ts TYPE timestamp,
            tl TYPE timestampl,
          END OF timestamp_line,
          timestamp  LIKE timestamp_line,
          timestamps LIKE STANDARD TABLE OF timestamp_line.

    DO 100000 TIMES.
      GET TIME STAMP FIELD timestamp-ts.
      GET TIME STAMP FIELD timestamp-tl.
      APPEND timestamp TO timestamps.
    ENDDO.

    DATA(start1) = start_measurement( ).
    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data          = timestamps
        ts_as_iso8601 = abap_true
      RECEIVING
        r_json        = DATA(json1).
    DATA(end1) = end_measurement( ).

    DATA(start2) = start_measurement( ).
    CALL METHOD z_ui2_json=>serialize
      EXPORTING
        data          = timestamps
        ts_as_iso8601 = abap_true
      RECEIVING
        r_json        = DATA(json2).
    DATA(end2) = end_measurement( ).

    store_runtime( name = `Timestamp` old = end1 - start1 new = end2 - start2 ).

    IF ( json1 NE json2 ).
      RAISE EXCEPTION TYPE z_ex_json_perf.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
