*-----------------------------------------------------------------------------
* <Rating>XXX</Rating>
*-----------------------------------------------------------------------------
    PROGRAM EPDMP
*-----------------------------------------------------------------------------
* Einar's little data extraction tool
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.PGM.FILE
    $INSERT I_F.STANDARD.SELECTION
    $INSERT I_F.LOCAL.REF.TABLE
    $INSERT I_F.LOCAL.TABLE

* Parse arguments and instruct the user in case of no success
    app_arg = @SENTENCE[' ',1,1]    
    ret_msg = "Try ":app_arg:" (-(h<ead>|t<ail>|r<andom>|s<ummary>)(N)) FILE (WITH CLAUSE)" 
    sample_spec = '' ; sample_size = '' ; file_name = '' ; with_clause = ''
    GOSUB parseArgs
    IF ret_msg THEN GOSUB endProgram

* Try opening the file to be dumped and related files
    file_p = '' ; app_name = ''
    GOSUB loadFiles
    IF ret_msg THEN GOSUB endProgram

* Select the records as specified
    rows = '' ; n_rows = 0
    GOSUB selectRecords
    IF ret_msg THEN GOSUB endProgram

* Output contents of core, local, and dynamic fields
    header = '' ; n_cols = 0 ; fsep = CHARX(9)
    local_ref_pos = 0 ; n_local_ref = 0 ; usr_field_pos = 0 ; n_usr_field = 0
    GOSUB fetchHeader
    IF sample_spec EQ 'S' THEN
        GOSUB fetchRows
        GOSUB displaySummary
    END ELSE
        IF header THEN CRT CHANGE(FIELDS(header, '%', 1, 1), @FM, fsep)
        GOSUB fetchRows
    END

    STOP
*-----------------------------------------------------------------------------
parseArgs:

* Make sense of options if present
    arg_pos = 2
    IF LEFT(@SENTENCE[' ',arg_pos,1], 1) EQ '-' THEN
        option_arg = UPCASE(FIELD(@SENTENCE[' ',arg_pos,1], '-', 2, 1))
        IF option_arg*1 THEN option_arg = 'H':option_arg
        
        sample_spec = option_arg[1,1]
        IF sample_spec[1,1] MATCHES 'H':@VM:'T':@VM:'R':@VM:'S' ELSE
            CRT app_arg:": Unknown option '":option_arg:"'."
            RETURN
        END
        
        sample_size = option_arg[2,LEN(option_arg)-1]
        IF sample_size THEN
            IF sample_size*1 ELSE
                CRT app_arg:": Sample size '":sample_size:"' not numeric."
                RETURN
            END
        END ELSE
            sample_size = 6
        END
        
        arg_pos++
    END
    
* File to be dumped mandatory
    file_name = @SENTENCE[' ',arg_pos,1]
    IF file_name ELSE
        CRT app_arg:": FILE not specified."
        RETURN
    END

* Record selection criteria may be provided
    arg_pos++
    with_clause = @SENTENCE[' ',arg_pos,DCOUNT(@SENTENCE, ' ')]
    IF with_clause AND with_clause[1,4] NE 'WITH' THEN
        CRT app_arg:": Incorrect selection criteria '":with_clause:"'."
        RETURN
    END
    
* Arguments successfully parsed, return no instructions
    ret_msg = ''
    
    RETURN
*-----------------------------------------------------------------------------
loadFiles:

* Validate the file name with company prefix
    BEGIN CASE
    CASE file_name[1,2] EQ 'F.'
        company = 'BNK'
        app_name = FIELD(file_name[3,LEN(file_name)], '$', 1, 1)
    CASE LEN(file_name['.',1,1]) EQ 4 AND file_name[1,1] EQ 'F'
        company = file_name[2,3]
        app_name = FIELD(file_name[6,LEN(file_name)], '$', 1, 1)
    CASE OTHERWISE
        ret_msg = file_name:' is not a valid file name'
        RETURN
    END CASE

* Attempt to open the file to be dumped and fetch its metadata  
    OPEN file_name TO file_p THEN
        IF sample_spec EQ 'S' THEN
            OPEN 'F.PGM.FILE' TO pf_p THEN
                READ r_pf FROM pf_p,app_name THEN
                    CRT r_pf<EB.PGM.SCREEN.TITLE,1>
                END
            END
        END                    
    END ELSE
        ret_msg = 'File ':file_name:' does not exist'
    END

    RETURN
*-----------------------------------------------------------------------------
selectRecords:

* Select quick sample of records if possible
    IF sample_spec MATCHES 'H' AND NOT(with_clause) THEN
        SELECT file_p
        
        LOOP
            READNEXT recid ELSE recid = ''
        WHILE recid NE '' AND n_rows LT sample_size
            rows<-1> = recid
            n_rows++
        REPEAT

* Select all records corresponding to criteria
    END ELSE
        selc = 'SSELECT ':file_name
        IF with_clause THEN selc := ' ':with_clause
        EXECUTE selc SETTING exec_msg CAPTURING exec_outp
        IF NOT(@SYSTEM.RETURN.CODE) THEN
            IF exec_msg<1,3> NE 'QLNUMESL' AND exec_msg<1,2> NE 'QLNONSEL' THEN
                ret_msg = "Incorrect SELECT statement '":selc:"'"
                RETURN
            END
        END
        READLIST rows ELSE rows = ''

        n_rows = @SELECTED
        IF sample_size AND sample_size LT n_rows THEN
            BEGIN CASE
            CASE sample_spec[1,1] EQ 'T'
                rows = rows[@FM,n_rows-sample_size,sample_size]
            CASE sample_spec[1,1] EQ 'R'
                random_rows = ''
                FOR i = 1 TO sample_size
                    LOOP r_id = RND(n_rows) + 1 WHILE r_id MATCHES LOWER(random_rows) REPEAT
                    random_rows<i> = rows<r_id>
                NEXT i
                rows = random_rows
            CASE OTHERWISE
                rows = rows[@FM,1,sample_size]
            END CASE

            n_rows = sample_size
        END
    END

    RETURN
*-----------------------------------------------------------------------------
fetchHeader:

* Build header from STANDARD.SELECTION record if possible
    OPEN 'F.STANDARD.SELECTION' TO ss_p THEN
        READ r_ss FROM ss_p,app_name THEN
            V$FUNCTION = 'XX'
            CALL @app_name
            CALL LOAD.COMPANY(company)
        END ELSE
            RETURN
        END
    END

* Fetch system, local, and audit field names respectively
    FOR f_idx = 0 TO C$SYSDIM
        LOCATE f_idx IN r_ss<SSL.SYS.FIELD.NO,1> SETTING pos THEN
            ffmt = r_ss<SSL.SYS.DISPLAY.FMT,pos>
            col_type = ffmt[1,LEN(ffmt)-1]:FIELD(r_ss<SSL.SYS.VAL.PROG,pos>['IN2',2,1], '&', 1, 1)
        END ELSE
            BREAK
        END
    
        BEGIN CASE
        CASE f_idx EQ 0
            header = 'RECID%':col_type
        CASE F(f_idx) AND F(f_idx) NE 'XX.LOCAL.REF'
            header<-1> = F(f_idx):'%':col_type
        CASE F(f_idx) EQ 'XX.LOCAL.REF'
            OPEN 'F.LOCAL.REF.TABLE' TO lrt_p THEN
                READ r_lrt FROM lrt_p,app_name THEN
                    OPEN 'F.LOCAL.TABLE' TO lt_p THEN
                        n_local_ref = DCOUNT(r_lrt<EB.LRT.LOCAL.TABLE.NO>, @VM)
                        FOR i = 1 TO n_local_ref
                            READ r_lt FROM lt_p,r_lrt<EB.LRT.LOCAL.TABLE.NO,i> THEN
                                col_name = r_lrt<EB.LRT.SUB.ASSOC.CODE,i>:'L_':r_lt<EB.LTA.SHORT.NAME>
                                col_type = r_lt<EB.LTA.MAXIMUM.CHAR>:r_lt<EB.LTA.CHAR.TYPE>
                                header<-1> = col_name:'%':col_type
                            END
                        NEXT i
                    END
                END ELSE
                    header<-1> = 'XX.LOCAL.REF%':col_type
                END
            END
            local_ref_pos = f_idx
        CASE OTHERWISE
            IF r_ss<SSL.SYS.SINGLE.MULT,pos> EQ 'M' THEN
                header<-1> = 'XX-':r_ss<SSL.SYS.FIELD.NAME,pos>:'%':col_type
            END ELSE
                header<-1> = r_ss<SSL.SYS.FIELD.NAME,pos>:'%':col_type
            END
        END CASE
    NEXT f_idx

* Add user fields
    usr_field_pos = DCOUNT(header, @FM) + 1
    n_usr_field = DCOUNT(r_ss<SSL.USR.FIELD.NAME>,@VM)
    FOR i = 1 TO n_usr_field
        IF LEFT(r_ss<SSL.USR.FIELD.NO,i>,10) NE 'LOCAL.REF<' THEN
            col_name = r_ss<SSL.USR.TYPE,i>:'_':r_ss<SSL.USR.FIELD.NAME,i>
            ffmt = r_ss<SSL.USR.DISPLAY.FMT,i>
            col_type = ffmt[1,LEN(ffmt)-1]:FIELD(r_ss<SSL.USR.VAL.PROG,i>['IN2',2,1], '&', 1, 1)
            header<-1> = col_name:'%':col_type
        END
    NEXT i
    
    RETURN
*-----------------------------------------------------------------------------
fetchRows:
    
* Fetch all selected records
    FOR r_idx = 1 TO n_rows
        r_id = rows<r_idx>

* Fetch system fields
        READ r_app FROM file_p,r_id THEN
            row = r_id:@FM:r_app
        END

* Fetch local ref fields (not all values might have been populated)
        IF local_ref_pos THEN
            local_ref_fields = ''
            FOR i = 1 TO n_local_ref
                local_ref_fields<i> = r_app<local_ref_pos,i>
            NEXT i
            row<local_ref_pos+1> = local_ref_fields
        END

* Fetch user fields
        usr_field_idx = 0
        FOR i = 1 TO n_usr_field
            IF LEFT(r_ss<SSL.USR.FIELD.NO,i>,10) NE 'LOCAL.REF<' THEN
                CALL IDESC(file_name, r_id, r_app, r_ss<SSL.USR.FIELD.NAME,i>, idesc_val)
                row<usr_field_pos+usr_field_idx> = idesc_val
                usr_field_idx++
            END
        NEXT i

* Output a row (TODO: Perhaps the separators should be arguments as well)
        row = CHANGE(CHANGE(CHANGE(row, @FM, fsep), @VM, '\\'), @SM, '\')
        IF sample_spec EQ 'S' THEN
            rows<r_idx> = row
        END ELSE
            CRT row
        END
    NEXT r_idx

    RETURN
*-----------------------------------------------------------------------------
displaySummary:

* Build header from rows if no STANDARD.SELECTION available
    IF header THEN
        n_cols = DCOUNT(header, @FM)
    END ELSE
        n_cols = MAXIMUM(COUNTS(rows, fsep)) + 1
        
        col_type = 'tbd'
        header = 'RECID%':col_type
        FOR idx = 1 TO n_cols - 1
            header<-1> = 'F':idx:'%':col_type
        NEXT idx
    END
    CRT @SELECTED:" rows x ":n_cols:" columns"      

* Space allocated for column description    
    col_max_len = MAXIMUM(LENS(header)) + 6

* Output column descriptions and row samples    
    FOR c_idx = 1 TO n_cols
        col_no_name = FMT(c_idx, 'R*3'):' ':header<c_idx>['%',1,1]
        col_type = '<':header<c_idx>['%',2,1]:'>'
        col_space = SPACE(col_max_len - LEN(col_no_name) - LEN(col_type))

        vals = FIELDS(rows, fsep, c_idx, 1)
        
        CRT col_no_name:col_space:col_type:' ':CHANGE(vals, @FM, ', ')[1,100]
    NEXT c_idx
    
    RETURN
*-----------------------------------------------------------------------------
endProgram:

    CRT ret_msg

    END
