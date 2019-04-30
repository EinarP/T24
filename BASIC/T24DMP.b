*-----------------------------------------------------------------------------
* <Rating>588</Rating>
*-----------------------------------------------------------------------------
    PROGRAM T24DMP
*-----------------------------------------------------------------------------
* Extracts T24 file contents with local and user fields
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STANDARD.SELECTION
    $INSERT I_F.LOCAL.REF.TABLE
    $INSERT I_F.LOCAL.TABLE

    ret_msg = ''; fsep = CHARX(9)

    file_p = ''; app_name = ''; file_name = @SENTENCE[' ', 2, 1]
    GOSUB loadFiles
    IF NOT(ret_msg) THEN

        rows = ''; n_rows = 0
        sample_spec = @SENTENCE[' ', 3, DCOUNT(@SENTENCE, ' ')]
        GOSUB selectRecords
        IF NOT(ret_msg) THEN

            header = '@ID'
            local_ref_pos = 0; n_local_ref = 0; usr_field_pos = 0; n_usr_field = 0
            GOSUB fetchHeader

            FOR r_idx = 1 TO n_rows
                GOSUB fetchRow
            NEXT r_idx

            result = CHANGE(header, @FM, fsep):@FM:rows
            GOSUB saveResult
        END
    END

    PRINT ret_msg

    STOP
*-----------------------------------------------------------------------------
loadFiles:

* File to be dumped (with company prefix) mandatory
    IF NOT(file_name) THEN
        ret_msg = "Usage: T24DMP FILE (((H)N|RN|TN|'WITH CLAUSE')"
        RETURN
    END

* Validate the file name
    BEGIN CASE
    CASE file_name[1,2] EQ 'F.'
        company = 'BNK'
        app_name = file_name[3,LEN(file_name)]
    CASE file_name[1,5] MATCHES 'F3A.'
        company = file_name[2,3]
        app_name = file_name[6,LEN(file_name)]
    CASE OTHERWISE
        ret_msg = file_name:' is not a valid file name'
        RETURN
    END CASE

* Attempt to open the file to be dumped and fetch its metadata
    CALL CHECK.ROUTINE.EXIST(app_name, routine_exists, ret_info)
    IF routine_exists THEN
        OPEN file_name TO file_p THEN
            OPEN 'F.STANDARD.SELECTION' TO ss_p THEN
                READ r_ss FROM ss_p,app_name THEN
                    V$FUNCTION = 'XX'
                    CALL @app_name
                END
            END
            CALL LOAD.COMPANY(company)
        END ELSE
            ret_msg = 'File ':file_name:' does not exist'
        END
    END ELSE
        ret_msg = 'Application ':app_name:' does not exist'
    END

    RETURN
*-----------------------------------------------------------------------------
selectRecords:

* Number of records to dump (all if not specified) and how
    sample_size = ''

    IF sample_spec THEN
        IF sample_spec*1 THEN sample_spec = 'H':sample_spec

        BEGIN CASE
        CASE sample_spec[1,1] MATCHES 'H':@VM:'T':@VM:'R'
            sample_size = sample_spec[2,LEN(sample_spec)-1]
            IF NOT(sample_size*1) THEN
                ret_msg = 'Sample size must be numeric'
            END
        CASE sample_spec[1,4] MATCHES 'WITH'
 
        CASE OTHERWISE
            ret_msg = 'Unknown sample spec'
            RETURN
        END CASE
    END

* TODO: Select error handling in building record id list
    selc = 'SSELECT ':file_name
    IF sample_spec[1,4] EQ 'WITH' THEN selc := ' ':sample_spec
    CALL EB.READLIST(selc, rows, '', n_rows, ret_code)

* Subset to tail, random, or head (default) records if requested
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

    RETURN
*-----------------------------------------------------------------------------
fetchHeader:

* Fetch system, local, and audit field names respectively
    FOR f_idx = 1 TO C$SYSDIM
        BEGIN CASE
        CASE F(f_idx) AND F(f_idx) NE 'XX.LOCAL.REF'
            header<-1> = F(f_idx)
        CASE F(f_idx) EQ 'XX.LOCAL.REF'
            OPEN 'F.LOCAL.REF.TABLE' TO lrt_p THEN
                READ r_lrt FROM lrt_p,app_name THEN
                    OPEN 'F.LOCAL.TABLE' TO lt_p THEN
                        n_local_ref = DCOUNT(r_lrt<EB.LRT.LOCAL.TABLE.NO>, @VM)
                        FOR i = 1 TO n_local_ref
                            READ r_lt FROM lt_p,r_lrt<EB.LRT.LOCAL.TABLE.NO,i> THEN
                                sac = r_lrt<EB.LRT.SUB.ASSOC.CODE,i>
                                header<-1> = sac:'L_':r_lt<EB.LTA.SHORT.NAME>
                            END
                        NEXT i
                    END
                END ELSE
                    header<-1> = 'XX.LOCAL.REF'
                END
            END
            local_ref_pos = f_idx
        CASE OTHERWISE
            LOCATE f_idx IN r_ss<SSL.SYS.FIELD.NO,1> SETTING pos THEN
                IF r_ss<SSL.SYS.SINGLE.MULT,pos> EQ 'M' THEN
                    header<-1> = 'XX-':r_ss<SSL.SYS.FIELD.NAME,pos>
                END ELSE
                    header<-1> = r_ss<SSL.SYS.FIELD.NAME,pos>
                END
            END ELSE
                BREAK
            END
        END CASE
    NEXT f_idx

* Add user field names
    usr_field_pos = DCOUNT(header, @FM) + 1
    n_usr_field = DCOUNT(r_ss<SSL.USR.FIELD.NAME>,@VM)
    FOR i = 1 TO n_usr_field
        IF LEFT(r_ss<SSL.USR.FIELD.NO,i>,10) NE 'LOCAL.REF<' THEN
            header<-1> = r_ss<SSL.USR.TYPE,i>:'_':r_ss<SSL.USR.FIELD.NAME,i>
        END
    NEXT i

    RETURN
*-----------------------------------------------------------------------------
fetchRow:

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

* TODO: Perhaps separators should be arguments as well
    rows<r_idx> = CHANGE(CHANGE(CHANGE(row, @FM, fsep), @VM, '{vm}'), @SM, '{sm}')

    RETURN
*-----------------------------------------------------------------------------
saveResult:

    OPEN '&SAVEDLISTS&' TO sl_p THEN
        outp_file = 'TAD_':file_name:'.txt'
        WRITE result TO sl_p,outp_file
        ret_msg = file_name:' dumped into &SAVEDLISTS&/':outp_file:' (':n_rows:' records)'
    END ELSE
        ret_msg = 'Unable to output into &SAVEDLISTS&'
    END

    RETURN
